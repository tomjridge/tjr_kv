(** {2 Archived dummy Dmap and dmap_thread; do not use } *)

(*
open Tjr_monad.With_lwt
open Kv_intf
open Lwt_aux  (* provides various msg queues *)

open Config
open Kv_profilers

module S = struct
    type k = string
    let compare: k -> k -> int = Pervasives.compare
    type v = string
end
open S

module Queues = Lwt_aux.Make_queues(S)
open Queues

(* NOTE queues are mutable *)
let q_lru_pc_state = q_lru_pc.initial_state
let q_dmap_bt_state = q_dmap_bt.initial_state

(** {2 Simple freespace impl using an incrementing int ref} *)

module Alloc = struct
let fv =
  let x = ref 0 in
  fun () -> (x:=!x+1; !x)
end
open Alloc

module Dmap' : sig
  val dmap_thread :
    yield:(unit -> unit Lwt.t) ->
    sleep:(float -> unit Lwt.t) -> unit -> ('a, lwt) m
end = struct

  open Dmap_profiler
  let [d2b_aa   ;d2b_ab   ;d2b_ca   ;d2b_cb   ;dmap_l2d_deq1   ;dmap_l2d_deq2   ;dmap_es] = 
      ["d2b:aa" ;"d2b:ab" ;"d2b:ca" ;"d2b:cb" ;"dmap:l2d.deq1" ;"dmap:l2d.deq2" ;"dmap_es"] 
    |> List.map allocate_int 
  [@@warning "-8"]

  open Dmap_types

  (** Now we fill in the missing components: [dmap_ops,
     dmap_blocks_limit, bt_find, bt_detach].

      For the time being, we would like to use a dummy implementation
     of dmap_ops *)


  let dmap_state : (k,v,'ptr)Dmap_dummy_implementation.Dummy_state.t ref = 
    Dmap_dummy_implementation.Dummy_state.init_dummy_state ~init_ptr:0 
    |> ref
    
  let _ = dmap_state

  let with_state f = 
    f 
      ~state:(!dmap_state)
      ~set_state:(fun x -> dmap_state:=x; return ())

  (** NOTE the following enqueues a find event on the msg queue, and
     constructs a promise that waits for the result *)
  let bt_find = fun k ->
    event_ops.ev_create () >>= fun ev ->
    let callback = fun v -> event_ops.ev_signal ev v in
    mark d2b_aa; 
    q_dmap_bt.ops.memq_enqueue
      ~q:q_dmap_bt_state
      ~msg:Msg_dmap_bt.(Find(k,callback)) >>= fun () ->
    mark d2b_ab; 
    event_ops.ev_wait ev

  let bt_handle_detach (detach_info:('k,'v,'ptr)detach_info) =
    (* Printf.printf "bt_handle_detach start\n%!"; *)
    let kv_op_map = Tjr_pcache.Op_aux.default_kvop_map_ops () in
    let kv_ops = detach_info.past_map |> kv_op_map.bindings |> List.map snd in
    mark d2b_ca; 
    q_dmap_bt.ops.memq_enqueue
      ~q:q_dmap_bt_state
      ~msg:Msg_dmap_bt.(Detach {
          ops=kv_ops;
          new_dmap_root=detach_info.current_ptr}) >>= fun _ ->
    (* Printf.printf "bt_handle_detach end\n%!"; *)
    mark d2b_cb; 
    return ()

  let dmap_ops = 
    (* FIXME we really want an implementation that uses polymap, so we can convert to a map *)
    let raw_dmap_ops (* pcache_ops *) : ('op,'map,'ptr,'t)dmap_ops = 
        Dmap_dummy_implementation.make_dmap_ops
          ~monad_ops
          ~ops_per_block:dmap_ops_per_block
          ~alloc_ptr:(fun () -> return (fv()))
          ~with_state:{with_state}
    in
    let _ : (k,v,'ptr,'t) dmap_ops = raw_dmap_ops in      
    Dmap_with_blocks_limit.make_ops
    ~monad_ops
    ~dmap_ops:raw_dmap_ops
    ~dmap_blocks_limit
    ~bt_find
    ~bt_handle_detach

  let dmap_thread ~yield ~sleep () = 
    let loop_evictees = 
      let rec loop es = 
        from_lwt(yield ()) >>= fun () ->
        (* Printf.printf "dmap_thread, loop_evictees\n%!"; *)
        match es with
        | [] -> return ()
        | (k,e)::es -> 
          let open Im_intf in
          (* let open Mt_intf in *)
          match e with
          | Insert { value=v; _ } -> 
            dmap_ops.insert k v >>= fun () ->
            loop es
          | Delete _ -> 
            dmap_ops.delete k >>= fun () ->
            loop es
          | Lower _ -> 
            Printf.printf "WARNING!!! unexpected evictee: Lower\n%!"; 
            assert(false) (* should never happen FIXME? *)
            (* loop es *)
            (* FIXME perhaps define a restricted type *)
      in
      loop
    in
    let rec read_and_dispatch () =
      (* FIXME do we need to yield if we are simply dequeueing? *)
      (* FIXME why is yield coerced to from_lwt? should be monad-agnostic *)
      from_lwt(yield ()) >>= fun () ->
      (* Printf.printf "dmap_thread read_and_dispatch starts\n%!"; *)
      mark dmap_l2d_deq1;
      q_lru_pc.ops.memq_dequeue q_lru_pc_state >>= fun msg ->
      mark dmap_l2d_deq2;
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
      | Evictees es -> 
        mark dmap_es;
        loop_evictees es >>= fun () ->
        read_and_dispatch ()
    in
    read_and_dispatch ()
        
end
*)
