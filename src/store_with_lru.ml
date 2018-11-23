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

(* TODO

- a functional approach would be nicer (rather than maintaining refs); this likely involves using a new monad, a combination of lwt and state passing

*)

open Tjr_monad.Types
open Tjr_mem_queue.Types


(* configuration parameters ----------------------------------------- *)

open Config
let { lru_max_size; lru_evict_count; dcl_ops_per_block; pcache_blocks_limit; dcl_thread_delay; bt_thread_delay; _ } = 
  config

(* lwt ops ---------------------------------------------------- *)

(* see lwt_aux *)

open Lwt_aux


(* blk_id ----------------------------------------------------------- *)

(* open Blk_id_type *)


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
        ~max_size:lru_max_size
        ~evict_count:lru_evict_count
        ~compare_k:(fun (x:int) y -> Pervasives.compare x y))

  (* An empty message queue. NOTE not in funstore *)
  (* let q_lru_dcl = Lwt_aux.empty_queue () *)
  
  let enqueue msg = 
    q_lru_dcl_ops.enqueue ~q:q_lru_dcl ~msg


  let with_lru_ops : ('msg,'k,int,'t)with_lru_ops = 
    let with_lru f = 
      from_lwt (Lwt_mutex.lock lru_lock) >>= fun () ->
      f ~lru:(!lru_state) ~set_lru:(fun lru ->
          lru_state:=lru; return ()) >>= fun a ->
      (Lwt_mutex.unlock lru_lock; return a)
    in
    { with_lru }


  let lru_callback_ops () : (int,int,lwt) mt_callback_ops = 
    make_lru_callback_ops ~monad_ops ~async ~with_lru_ops  ~to_lower:enqueue

  let _ = lru_callback_ops

  (* the interface we expose to upper levels; add a dummy arg so that
     in test code we can set up profilers ie nothing gets executed at
     this point *)
  let lru_ops () = 
    Tjr_lru_cache.Multithreaded_lru.make_lru_ops 
      ~monad_ops 
      ~event_ops
      ~callback_ops:(lru_callback_ops ())

  let _ : unit -> (int,int,lwt) mt_ops = lru_ops

  let msg2string = 
    let open Lru_dcl_msg_type in
    function
    | Insert(k,v,_) -> Printf.sprintf "Insert(%d,%d)" k v
    | Delete(k,_) -> Printf.sprintf "Delete(%d)" k
    | Find(k,_) -> Printf.sprintf "Find(%d)" k
    | Evictees es -> Printf.sprintf "Evictees(len=%d)" (List.length es)

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
        (* Printf.printf "dcl_thread, maybe_roll_up\n%!"; *)
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
  let dcl_state = 
    Dcl_dummy_implementation.initial_dummy_state ~ptr:0 |> ref

  let _ = dcl_state

  let with_state f = 
    f 
      ~state:(!dcl_state)
      ~set_state:(fun x -> dcl_state:=x; return ())

  let pcache_ops : (int,int,'map,'ptr,'t)dcl_ops = 
    Dcl_dummy_implementation.make_ops
      ~monad_ops
      ~ops_per_block:dcl_ops_per_block
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


  let remdups ops =
    ops
    |> List.map (fun op -> (Ins_del_op_type.op2k op,op))    
    |> List.rev (* we want the most recent to be inserted into map last! *)
    (* FIXME perhaps add a `Most_recent_first tag *)
    |> fun kvs ->
    Tjr_list.with_each_elt
      ~list:kvs
      ~step:(fun ~state (k,v) -> Tjr_polymap.add k v state)
      ~init:(Tjr_polymap.empty Pervasives.compare)
    |> Tjr_polymap.bindings
    |> List.map snd

  let bt_handle_detach (detach_result:('ptr,detach_result_map)detach_result) =
    (* Printf.printf "bt_handle_detach start\n%!"; *)
    let kv_ops : (int,int) Ins_del_op_type.op list = 
      remdups detach_result.old_map 
    in
    (* Printf.printf "bt_handle_detach middle\n%!"; *)
    q_dcl_bt_ops.enqueue
      ~q:q_dcl_bt
      ~msg:Dcl_bt_msg_type.(Detach {
          ops=kv_ops;
          new_dcl_root=detach_result.new_ptr}) >>= fun _ ->
    (* Printf.printf "bt_handle_detach end\n%!"; *)
    return ()



  let dcl_ops = make_dcl_ops
    ~monad_ops
    ~pcache_ops
    ~pcache_blocks_limit
    ~bt_find
    ~bt_handle_detach

  let dcl_thread ~yield ~sleep = 
    let loop_evictees = 
      let open Tjr_lru_cache.Entry in
      let rec loop es = 
        from_lwt(yield ()) >>= fun () ->
        (* Printf.printf "dcl_thread, loop_evictees\n%!"; *)
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
          | Lower _ -> 
            (* Printf.printf "WARNING!!! unexpected evictee: Lower\n%!"; *)
            loop es
                         (* FIXME perhaps define a restricted type *)
      in
      loop
    in
    let rec read_and_dispatch () =
      from_lwt(yield ()) >>= fun () ->
      (* Printf.printf "dcl_thread read_and_dispatch starts\n%!"; *)
      q_lru_dcl_ops.dequeue ~q:q_lru_dcl >>= fun msg ->
      (* Printf.printf "dcl_thread dequeued: %s\n%!" (Lru'.msg2string msg); *)
      (* FIXME the following pause seems to require that the btree
         thread makes progress, but of course it cannot since there
         are no msgs on the queue *)
      from_lwt(sleep dcl_thread_delay) >>= fun () ->  (* FIXME *)
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

module Btree' = struct
  open Btree_ops_type
  open Dummy_btree_implementation
  open Tjr_pcache.Ins_del_op_type

  let state = ref (empty_btree ())
  let with_state f = 
    f 
      ~state:(!state)
      ~set_state:(fun x -> state:=x; return ())
  
  let btree_ops : (int,int,bt_ptr,lwt) btree_ops = 
    make_dummy_btree_ops ~monad_ops ~with_state:{with_state}


  open Dcl_bt_msg_type

  (** The thread listens at the end of the q_dcl_btree for msgs which it
   then runs against the B-tree, and records the new root pair. *)
  let btree_thread ~yield ~sleep = 
    let rec loop (ops:('k,'v)op list) = 
      from_lwt(yield()) >>= fun () ->
      match ops with
      | [] -> return ()
      | op::ops -> 
        (* FIXME more efficient if we dealt with multiple ops eg insert_many *)
        (* NOTE the following do not have callbacks, because they come
           from a flush from the pcache (even if the LRU user
           requested sync... the sync write is to the pcache) *)
        match op with
        | Insert(k,v) -> 
          btree_ops.insert k v >>= fun () ->
          loop ops
        | Delete k -> 
          btree_ops.delete k >>= fun () ->
          loop ops
    in
    let rec read_and_dispatch () =
      from_lwt(yield()) >>= fun () ->
      q_dcl_bt_ops.dequeue ~q:q_dcl_bt >>= fun msg ->
      from_lwt(sleep bt_thread_delay) >>= fun () ->  (* FIXME *)
      (* Printf.printf "btree_thread dequeued: %s\n%!" "-"; *)
      match msg with
      | Find(k,callback) ->
        btree_ops.find k >>= fun v ->
        async(fun () -> callback v) >>= fun () ->
        read_and_dispatch ()
      | Detach { ops; new_dcl_root } ->
        loop ops >>= fun () ->
        (* FIXME what to do with the new root? maybe nothing for the
           time being? *)
        (* FIXME what about root pair? *)
        btree_ops.sync () >>= fun ptr ->        
        Printf.printf 
          "New root pair: dcl_root=%d, bt_root=%d\n%!"
          new_dcl_root
          (ptr |> Ptr.t2int);
        read_and_dispatch ()
    in
    read_and_dispatch ()
end

