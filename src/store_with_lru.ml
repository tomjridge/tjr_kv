(** A KV store with the LRU frontend. *)

(** We construct:

- XXX Actually, work with references first XXX Functional store thread
  - this maintains the "global" state, including locks etc
  - the queues are kept separate since they are implemented using mutation anyway FIXME perhaps prefer a "functional" version using the functional store
  - includes:
    - lru_state
    - dmap_state
    - bt_state (a root pointer?)
  - also includes an Lwt_mvar for communication and implementation of with_state



- LRU
  - q_lru_dcl (msg queue from lru to dcl)
  - Lru (and lru_state)
  - Lru_t, which takes messages from lru.to_lower to enqueue on q_lru_dcl




- Dmap
  - q_dmap_bt (msg queue from dmap to btree)
  - Dmap and dmap_state
  - Dmap thread (Dmap_t)
    - takes msgs from q_dmap_btree and executes against dmap and B-tree


- B-tree
  - B-tree
  - B-tree thread (Btree_t) listening to q_dmap_btree
    - also includes root pair functionality



NOTE We refer to the combination of a Dmap and a B-tree as a RUM (for roll-up map).


*)

(* TODO

- a functional approach would be nicer (rather than maintaining refs); this likely involves using a new monad, a combination of lwt and state passing

*)

open Tjr_monad.With_lwt
open Kv_intf
open Lwt_aux  (* provides various msg queues *)

open Config
let { lru_max_size; lru_evict_count; dmap_ops_per_block;
      dmap_blocks_limit; dmap_thread_delay; bt_thread_delay; _ } =
  config


module Lru' = struct
         
  let from_lwt = With_lwt.from_lwt
  let to_lwt = With_lwt.to_lwt
         
  let lru_lock = Lwt_aux.create_mutex()
  let lru_state  : (int,int,_,_,_) lru_state ref = ref (
      Tjr_lru_cache.Mt_intf.mt_initial_state 
        ~max_size:lru_max_size
        ~evict_count:lru_evict_count
        (* FIXME pervasives.compare *)
        ~compare_k:(fun (x:int) y -> Pervasives.compare x y)) 

  let _ :
(int, int, (int, int Tjr_lru_cache.Im_intf.entry, unit) Tjr_map.map,
 (int, (int, With_lwt.lwt) blocked_thread list, unit) Tjr_map.map,
 With_lwt.lwt)
lru_state ref
= lru_state

  (* An empty message queue. NOTE not in funstore *)
  (* let q_lru_dcl = Lwt_aux.empty_queue () *)
  
  let enqueue msg = 
    q_lru_dmap_ops.memq_enqueue ~q:q_lru_dmap ~msg


  let with_lru_ops (* : ('msg,'k,int,'t)with_lru_ops *) = 
    let with_lru f = 
      from_lwt (Lwt_mutex.lock lru_lock) >>= fun () ->
      f ~lru:(!lru_state) ~set_lru:(fun lru ->
          lru_state:=lru; return ()) >>= fun a ->
      (Lwt_mutex.unlock lru_lock; return a)
    in
    { with_lru }


  let lru_callback_ops () : (int,int,lwt) Mt_callback_ops.mt_callback_ops = 
    make_lru_callback_ops ~monad_ops ~async ~with_lru_ops  ~to_lower:enqueue

  let _ = lru_callback_ops

  (* the interface we expose to upper levels; add a dummy arg so that
     in test code we can set up profilers ie nothing gets executed at
     this point *)
  let lru_ops () = 
    make_lru_ops 
      ~monad_ops 
      ~event_ops
      ~callback_ops:(lru_callback_ops ())

  let _ : unit -> (int,int,lwt) mt_ops = lru_ops

  let msg2string = 
    let open Msg_lru_dmap in
    function
    | Insert(k,v,_) -> Printf.sprintf "Insert(%d,%d)" k v
    | Delete(k,_) -> Printf.sprintf "Delete(%d)" k
    | Find(k,_) -> Printf.sprintf "Find(%d)" k
    | Evictees es -> Printf.sprintf "Evictees(len=%d)" (List.length es)

end


(** {2 simple freespace} *)

module Alloc = struct
let fv =
  let x = ref 0 in
  fun () -> (x:=!x+1; !x)
end
open Alloc


(** {2 DMAP and dmap_thread } *)

module Dmap' = struct

  open Tjr_pcache
  open Dmap_types

  (** 

Construct the DMAP, which uses the dmap_ops and wraps it in a routine which occasionally executes a B-tree roll-up. We call this a "RUM" (roll-up map).

Parameters:

- [monad_ops]

- [dmap_ops]: dmap from tjr_pcache, with dmap interface

- [dmap_blocks_limit]: how many blocks in the pcache before
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
  let make_rum_ops
      ~monad_ops 
      ~(dmap_ops:('k,'v,'ptr,'t) Dmap_types.dmap_ops)
      ~dmap_blocks_limit 
      ~bt_find
      ~(bt_handle_detach:('k,'v,'ptr) Dmap_types.detach_info -> (unit,'t)m)
    (* : ('k,'v,'t) Tjr_fs_shared.Map_ops.map_ops  *)
    =
    (* let open Mref_plus in *)
    let ( >>= ) = monad_ops.bind in
    let return = monad_ops.return in
    let pc = dmap_ops in  (* persistent cache; another name for dmap *)
    let find k = 
      pc.find k >>= fun v ->
      match v with
      | None -> bt_find k
      | Some v -> return (Some v)
    in
    let maybe_roll_up () = 
      pc.block_list_length () >>= fun n ->
      match n >= dmap_blocks_limit with
      | false -> return `No_roll_up_needed
      | true -> 
        (* Printf.printf "dmap_thread, maybe_roll_up\n%!"; *)
        pc.detach () >>= fun detach_result ->
        bt_handle_detach detach_result >>= fun () ->
        return `Ok
    in
    let insert k v =
      pc.insert k v >>= fun () -> 
      maybe_roll_up () >>= fun _ ->
      return ()
    in
    let delete k =
      pc.delete k >>= fun () -> 
      maybe_roll_up () >>= fun _ ->
      return ()
    in
    let _insert_many k v kvs = 
      (* FIXME we should do something smarter here *)
      insert k v >>= fun () -> return kvs
    in
    `Run (find, insert, delete) 

  let _ = make_rum_ops

  (** Now we fill in the missing components: [dmap_ops, dmap_blocks_limit, bt_find, bt_detach]. 

      For the time being, we
     would like to use a dummy implementation of dmap_ops *)
  let dmap_state = 
    Dmap_dummy_implementation.Dummy_state.init_dummy_state ~init_ptr:0 
    |> ref

  let _ = dmap_state

  let with_state f = 
    f 
      ~state:(!dmap_state)
      ~set_state:(fun x -> dmap_state:=x; return ())

  (* FIXME we really want an implementation that uses polymap, so we can convert to a map *)
  let dmap_ops (* pcache_ops *) : ('op,'map,'ptr,'t)dmap_ops = 
    Dmap_dummy_implementation.make_dmap_ops
      ~monad_ops
      ~ops_per_block:dmap_ops_per_block
      ~alloc_ptr:(fun () -> return (fv()))
      ~with_state:{with_state}
      
  let _ : (int,int,'ptr,'t) dmap_ops = dmap_ops

  (** NOTE the following enqueues a find event on the msg queue, and
     constructs a promise that waits for the result *)
  let bt_find = fun k ->
    event_ops.ev_create () >>= fun ev ->
    let callback = fun v -> event_ops.ev_signal ev v in
    q_dmap_bt_ops.memq_enqueue 
      ~q:q_dmap_bt 
      ~msg:Msg_dmap_bt.(Find(k,callback)) >>= fun () ->
    event_ops.ev_wait ev

  let bt_handle_detach (detach_info:('k,'v,'ptr)detach_info) =
    (* Printf.printf "bt_handle_detach start\n%!"; *)
    let kv_op_map = Tjr_pcache.Op_aux.default_kvop_map_ops () in
    let kv_ops = detach_info.past_map |> kv_op_map.bindings |> List.map snd in
    q_dmap_bt_ops.memq_enqueue
      ~q:q_dmap_bt
      ~msg:Msg_dmap_bt.(Detach {
          ops=kv_ops;
          new_dmap_root=detach_info.current_ptr}) >>= fun _ ->
    (* Printf.printf "bt_handle_detach end\n%!"; *)
    return ()

  let rum_ops = make_rum_ops
    ~monad_ops
    ~dmap_ops (* pcache_ops *)
    ~dmap_blocks_limit
    ~bt_find
    ~bt_handle_detach

  let dmap_thread ~yield ~sleep = 
    let loop_evictees = 
      (* let open Tjr_lru_cache.Entry in *)
      let rec loop es = 
        from_lwt(yield ()) >>= fun () ->
        (* Printf.printf "dmap_thread, loop_evictees\n%!"; *)
        match es with
        | [] -> return ()
        | (k,e)::es -> 
          let open Im_intf in
          let open Mt_intf in
          match e.entry_type with
          | Insert { value=v; _ } -> 
            dmap_ops.insert k v >>= fun () ->
            loop es
          | Delete _ -> 
            dmap_ops.delete k >>= fun () ->
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
      (* Printf.printf "dmap_thread read_and_dispatch starts\n%!"; *)
      q_lru_dmap_ops.memq_dequeue q_lru_dmap >>= fun msg ->
      (* Printf.printf "dmap_thread dequeued: %s\n%!" (Lru'.msg2string msg); *)
      (* FIXME the following pause seems to require that the btree
         thread makes progress, but of course it cannot since there
         are no msgs on the queue *)
      from_lwt(sleep dmap_thread_delay) >>= fun () ->  (* FIXME *)
      match msg with
      | Insert (k,v,callback) ->
        dmap_ops.insert k v >>= fun () -> 
        async (fun () -> callback ()) >>= fun () ->
        read_and_dispatch ()
      | Delete (k,callback) ->
        dmap_ops.delete k >>= fun () ->
        async (fun () -> callback ()) >>= fun () ->
        read_and_dispatch ()
      | Find (k,callback) -> 
        dmap_ops.find k >>= fun v ->
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


  open Dmap_bt_msg_type

  (** The thread listens at the end of the q_dmap_btree for msgs which it
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
      q_dmap_bt_ops.dequeue ~q:q_dmap_bt >>= fun msg ->
      from_lwt(sleep bt_thread_delay) >>= fun () ->  (* FIXME *)
      (* Printf.printf "btree_thread dequeued: %s\n%!" "-"; *)
      match msg with
      | Find(k,callback) ->
        btree_ops.find k >>= fun v ->
        async(fun () -> callback v) >>= fun () ->
        read_and_dispatch ()
      | Detach { ops; new_dmap_root } ->
        loop ops >>= fun () ->
        (* FIXME what to do with the new root? maybe nothing for the
           time being? *)
        (* FIXME what about root pair? *)
        btree_ops.sync () >>= fun ptr ->        
        Printf.printf 
          "New root pair: dmap_root=%d, bt_root=%d\n%!"
          new_dmap_root
          (ptr |> Ptr.t2int);
        read_and_dispatch ()
    in
    read_and_dispatch ()
end


(*
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
*)



(*  let dmap_as_dmap_ops = 
    Detachable_map.convert_dmap_ops_to_map_ops
      ~dmap_ops:dmap_ops
*)

