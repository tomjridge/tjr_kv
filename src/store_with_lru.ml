(** A KV store with the LRU frontend. *)

(** We construct:

- XXX Actually, work with references first XXX Functional store thread
  - this maintains the "global" state, including locks etc
  - the queues are kept separate since they are implemented using mutation anyway FIXME perhaps prefer a "functional" version using the functional store
  - includes:
    - lru_state
    - dcl_state
    - bt_state (a root pointer?)
  - also includes an Lwt_mvar for communication and implementation of with_state



- LRU
  - q_lru_dcl (msg queue from lru to dcl)
  - Lru (and lru_state)
  - Lru_t, which takes messages from lru.to_lower to enqueue on q_lru_dcl




- DCL
  - q_dcl_bt (msg queue from dcl to btree)
  - DCL (detachable chunked list) and dcl_state
  - DCL thread (Dcl_t)
    - takes msgs from q_dcl_btree and executes against dcl and B-tree


- B-tree
  - B-tree
  - B-tree thread (Btree_t) listening to q_dcl_btree
    - also includes root pair functionality



*)

open Tjr_monad.Types
open Tjr_mem_queue.Types


(* lwt ops ---------------------------------------------------- *)

(* see lwt_aux *)

open Lwt_aux


(* blk_id ----------------------------------------------------------- *)

open Blk_id_type


(* q_lru_dcl -------------------------------------------------------- *)

let q_lru_dcl,q_lru_dcl_ops = Lwt_aux.(q_lru_dcl,q_lru_dcl_ops)



(* lru -------------------------------------------------------------- *)


module Lru' = struct
  open Tjr_lru_cache.Multithreaded_lru

  let from_lwt = Tjr_monad.Lwt_instance.from_lwt
  let to_lwt = Tjr_monad.Lwt_instance.to_lwt
         
  let lru_lock = Lwt_aux.create_mutex()
  let lru_state : (int,int,lwt) lru_state ref = ref (
      Tjr_lru_cache.Mt_types.mt_initial_state 
        ~max_size:4 
        ~evict_count:2 
        ~compare_k:(fun (x:int) y -> Pervasives.compare x y))

  (* An empty message queue. NOTE not in funstore *)
  (* let q_lru_dcl = Lwt_aux.empty_queue () *)
  
  let enqueue msg = q_lru_dcl_ops.enqueue ~q:q_lru_dcl ~msg

  let with_lru_ops : ('msg,'k,int,'t)with_lru_ops = 
    let with_lru f = 
      from_lwt (Lwt_mutex.lock lru_lock) >>= fun () ->
      f ~lru:(!lru_state) ~set_lru:(fun lru ->
          lru_state:=lru; return ()) >>= fun a ->
      (Lwt_mutex.unlock lru_lock; return a)
    in
    { with_lru }


  let lru_callback_ops : (int,int,lwt) mt_callback_ops = 
    make_lru_callback_ops ~monad_ops ~with_lru_ops ~async

  let _ = lru_callback_ops

  (* the interface we expose to upper levels *)
  let lru_ops = 
    Tjr_lru_cache.Multithreaded_lru.make_lru_ops 
      ~monad_ops 
      ~event_ops
      ~callback_ops:lru_callback_ops

  let _ : (int,int,lwt) mt_ops = lru_ops


  let lru_thread () = 
    let rec enqueue_loop msgs =
      match msgs with
      | [] -> return ()
      | msg::msgs -> 
        enqueue msg >>= fun () -> enqueue_loop msgs
    in
    let rec enqueue_to_lower () =
      with_lru_ops.with_lru (fun ~lru ~set_lru ->
          let to_lower = List.rev lru.to_lower in
          set_lru {lru with to_lower=[]} >>= fun () ->
          return to_lower) >>= fun to_lower ->
      enqueue_loop to_lower >>= fun () ->
      enqueue_to_lower ()
    in
    enqueue_to_lower ()

end



(* q_dcl_bt ------------------------------------------------------ *)

let q_dcl_bt,q_dcl_bt_ops = Lwt_aux.(q_dcl_bt,q_dcl_bt_ops)


(* DCL and dcl_thread --------------------------------------------------- *)

module Dcl' = struct

  open Tjr_pcache
  open Tjr_pcache.Detachable_chunked_list

  type detach_result_map = (int,int) Ins_del_op_type.op list

  (** 

Construct the DCL. Parameters:

- [monad_ops]

- [pcache_ops]: dcl_ops from tjr_pcache

- [pcache_blocks_limit]: how many blocks in the pcache before
  attempting a roll-up; if the length of pcache is [>=] this limit, we
  attempt a roll-up; NOTE that this limit should be >= 2 (if we roll
  up with 1 block, then in fact nothing gets rolled up because we roll
  up "upto" the current block; not a problem but probably pointless
  for testing)

- [bt_find]: called if key not in pcache map FIXME do we need a
  write-through cache here? or just rely on the front-end LRU? FIXME
  note that even if a rollup is taking place, we can use the old
  B-tree root for the [bt_find] operation.

- [bt_handle_detach]: called to detach the rollup into another thread;
  typically this operation puts a msg on a message queue which is then
  received and acted upon by the dedicated rollup thread

  *)
  let make_dcl_ops
      ~monad_ops 
      ~pcache_ops 
      ~pcache_blocks_limit 
      ~bt_find
      ~(bt_handle_detach:('ptr,detach_result_map)detach_result -> (unit,'t)m)
    : ('k,'v,'t) Tjr_btree.Map_ops.map_ops 
    =
    (* let open Mref_plus in *)
    let ( >>= ) = monad_ops.bind in
    let return = monad_ops.return in
    let pc = pcache_ops in
    let find k = 
      pc.find k >>= fun op ->
      match op with
      | None -> bt_find k
      | Some op ->
        match op with
        | Insert(k,v) -> return (Some v)
        | Delete k -> return None
    in
    let maybe_roll_up () = 
      pc.block_list_length () >>= fun n ->
      match n >= pcache_blocks_limit with
      | false -> return `No_roll_up_needed
      | true -> 
        pc.detach () >>= fun detach_result ->
        bt_handle_detach detach_result >>= fun () ->
        return `Ok
    in
    let insert k v =
      pc.add (Insert(k,v)) >>= fun () -> 
      maybe_roll_up () >>= fun _ ->
      return ()
    in
    let delete k =
      pc.add (Delete k) >>= fun () -> 
      maybe_roll_up () >>= fun _ ->
      return ()
    in
    let insert_many k v kvs = 
      (* FIXME we should do something smarter here *)
      insert k v >>= fun () -> return kvs
    in
    { find; insert; delete; insert_many }

  let _ = make_dcl_ops

  (** Now we fill in the missing components: [pcache_ops,
     pcache_blocks_limit, bt_find, bt_detach]. For the time being, we
     would like to use a dummy implementation of pcache_ops *)
  let dcl_state = Dcl_dummy_implementation.initial_dummy_state ~ptr:0 |> ref

  let _ = dcl_state

  let with_state f = 
    f 
      ~state:(!dcl_state)
      ~set_state:(fun x -> dcl_state:=x; return ())

  let pcache_ops : (int,int,'map,'ptr,'t)dcl_ops = 
    Dcl_dummy_implementation.make_ops
      ~monad_ops
      ~ops_per_block:2
      ~new_ptr:(fun xs -> 1+Tjr_list.max_list xs)
      ~with_state:{with_state}
      
  let _ = pcache_ops

  (** NOTE the following enqueues a find event on the msg queue, and
     constructs a promise that waits for the result *)
  let bt_find = fun k ->
    event_ops.ev_create () >>= fun ev ->
    let callback = fun v -> event_ops.ev_signal ev v in
    q_dcl_bt_ops.enqueue 
      ~q:q_dcl_bt 
      ~msg:Dcl_bt_msg_type.(Find(k,callback)) >>= fun () ->
    event_ops.ev_wait ev



  let bt_handle_detach (detach_result:('ptr,detach_result_map)detach_result) =
    let kv_ops : (int,int) Ins_del_op_type.op list = 
      detach_result.old_map 
      |> List.map (fun op -> (Ins_del_op_type.op2k op,op))
      |> Tjr_list.assoc_list_remdups
      |> List.map snd
    in
    q_dcl_bt_ops.enqueue
      ~q:q_dcl_bt
      ~msg:Dcl_bt_msg_type.(Detach {
          ops=kv_ops;
          new_dcl_root=detach_result.new_ptr})


  let dcl_ops = make_dcl_ops
    ~monad_ops
    ~pcache_ops
    ~pcache_blocks_limit:3
    ~bt_find
    ~bt_handle_detach

  let dcl_thread () = 
    let loop_evictees = 
      let open Tjr_lru_cache.Entry in
      let rec loop es = 
        match es with
        | [] -> return ()
        | (k,e)::es -> 
          match e.entry_type with
          | Insert { value=v; _ } -> 
            dcl_ops.insert k v >>= fun () ->
            loop es
          | Delete _ -> 
            dcl_ops.delete k >>= fun () ->
            loop es
          | Lower _ -> failwith "unexpected evictee: Lower"
                         (* FIXME perhaps define a restricted type *)
      in
      loop
    in
    let rec read_and_dispatch () =
      q_lru_dcl_ops.dequeue ~q:q_lru_dcl >>= fun msg ->
      let open Tjr_lru_cache.Msg_type in
      match msg with
      | Insert (k,v,callback) ->
        dcl_ops.insert k v >>= fun () -> 
        async (fun () -> callback ()) >>= fun () ->
        read_and_dispatch ()
      | Delete (k,callback) ->
        dcl_ops.delete k >>= fun () ->
        async (fun () -> callback ()) >>= fun () ->
        read_and_dispatch ()
      | Find (k,callback) -> 
        dcl_ops.find k >>= fun v ->
        async (fun () -> callback v) >>= fun () ->
        read_and_dispatch ()
      | Evictees es -> loop_evictees es >>= fun () ->
        read_and_dispatch ()
    in
    read_and_dispatch ()
        
end


(* B-tree/btree ops/bt thread ------------------------------------------- *)

open Btree_ops

let btree_ops : (int,int,blk_id,lwt) btree_ops = 
  (* Btree_model.btree_ops ~monad_ops ~with_btree ~get_btree_root_and_incr *)
  failwith "FIXME"

(* FIXME what about root pair? *)



(* B-tree thread ---------------------------------------------------- *)

(** The thread listens at the end of the q_dcl_btree for msgs which it
   then runs against the B-tree, and records the new root pair. *)

(*
module Btree_t = struct
  (* open Msg *)
  open Tjr_mem_queue.Mem_queue

  let btree_t ~kvop_map_bindings ~sync_new_roots ~btree_ops ~q_ops ~q_dcl_btree = 
    let { find; insert=bt_insert; delete=bt_delete; sync=bt_sync } = btree_ops in
    let execute_btree_rollup = 
      Sync_store.execute_btree_rollup
        ~monad_ops
        ~bt_insert
        ~bt_delete
        ~bt_sync
        ~kvop_map_bindings
        ~sync_new_roots
    in
    let rec loop () =
      q_ops.dequeue ~q:q_dcl_btree >>= fun detach -> 
      execute_btree_rollup detach >>= fun () ->
      (* FIXME change ex to return new root *)
      loop ()
    in
    loop

  let _ = btree_t

end
*)






    
    



(* old ================================================================ *)

(*

(* sync store ------------------------------------------------------- *)

module Sync_store = Synchronous_store.Make(
  struct
    module Bt_blk_id = struct 
      type t = blk_id
      let int2t = fun i -> i 
      let t2int = fun ( i) -> i
    end
    module Pc_blk_id = struct 
      type t = blk_id
      let int2t = fun i ->  i 
      let t2int = fun ( i) -> i
    end
  end)





(* store ------------------------------------------------------------ *)



module Fun_store' = struct
  open Tjr_monad.Types
  open Fun_store

  (* FIXME probably not needed; used to enforce sequential access to
     the fun store *)
  let global_lock = create_mutex()


    
  let from_lwt = Tjr_monad.Lwt_instance.from_lwt

  type ('a,'b) mb_msg = 
    Set_lru of 'a
    | With_lru of 'b

  let mb : ('a,'b) mb_msg Lwt_mvar.t = Lwt_mvar.create_empty () 


  let fun_store_t ~async = 
    let rec loop state = 
      from_lwt (Lwt_mvar.take mb) >>= fun msg ->
      match msg with
      | Set_lru lru -> (
          set lru_state lru state |> loop)
      | With_lru kk -> (
          (* Because this is given to the calling thread, there needs
             to be a way to set the lru state from a non-funstore
             thread; so we just reuse the mbox; note that this relies
             on the call to set_lru being made asynchronously, so that
             the fun_store_t can service the set_lru request *)
          let set_lru lru = from_lwt (Lwt_mvar.put mb (Set_lru lru)) in
          from_lwt (Lwt_mutex.lock global_lock) >>= fun () ->
          from_lwt (Lwt_mutex.lock lru_lock) >>= fun () ->
          async(fun () ->
              (kk ~lru:(get lru_state state) ~set_lru:set_lru) >>= fun a ->
              (Lwt_mutex.unlock lru_lock; return ()));
          (Lwt_mutex.unlock global_lock; loop state))
    in
    fun state -> loop state

  let put msg = 
    from_lwt(Lwt_mvar.put msg mb)

  let _ = put

  

end
*)
