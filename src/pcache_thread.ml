(** Pcache worker thread, which takes an existing pcache_ops, wraps it
   using blocks limit, and interfaces with the B-tree *)

(* NOTE specific to lwt; necessary? *)
(* open Tjr_monad.With_lwt *)

(* open Shared_ctxt *)
open Kv_intf
open Kv_config_profilers

module P = Pvt_pcache_with_blocks_limit

module type S = sig
  type t
  val monad_ops : t monad_ops
  val event_ops : t event_ops
  val yield     : unit -> (unit,t)m
  val async     : t async
end

module Make(S:S) = struct
  module S = S
  open S

  let ( >>= ) = monad_ops.bind

  let return = monad_ops.return

  let make_pcache_thread (type k v ls kvop_map blk_id)
    ~(kvop_map_ops:(k,(k,v)kvop,kvop_map)Tjr_map.map_ops)
    ~pcache_blocks_limit
    ~(pcache_ops: _ pcache_ops)
    ~(q_lru_pc: _ q_lru_pc)
    ~(q_pc_bt: _ q_pc_bt)
    : < start_pcache_thread: unit -> (unit,t)m > 
  =
  let open (struct

    type nonrec pcache_ops = (k,v,blk_id,kvop_map,lwt) pcache_ops
    type nonrec pcache_state = (blk_id,kvop_map) Pcache_intf.pcache_state

    let file = "pct"

    let [mk1;mk2;mk3;mk4] = 
      ["1" ;"2" ;"3" ;"4"]
      |> List.map (fun s -> file^"_"^s)
      |> List.map intern
    [@@warning "-8"]

    let mark = pcache_profiler.mark

    (** Now we fill in the missing components: [bt_find,
       bt_handle_detach].*)

    (** NOTE this enqueues a find event on the msg queue, and
        constructs a promise that waits for the result *)
    let bt_find = fun k ->
      event_ops.ev_create () >>= fun ev ->
      let callback = fun v -> event_ops.ev_signal ev v in
      mark mk1; 
      q_pc_bt#enqueue (Find(k,callback)) >>= fun () ->
      mark (-1*mk1); 
      event_ops.ev_wait ev

    let bt_handle_detach (detach_info:('k,'v,'blk_id,'kvop_map)Detach_info.t) =
      Printf.printf "bt_handle_detach start\n%!";
      let kv_ops = detach_info.past_map |> kvop_map_ops.bindings |> List.map snd in
      mark mk2; 
      q_pc_bt#enqueue
        Msg_pc_bt.(Detach {
            ops=kv_ops;
            new_pcache_hd_tl=detach_info.current_ptr}) >>= fun _ ->
      mark (-1*mk2); 
      return ()

    let _ = bt_handle_detach

    let pcache_op_count = ref 0

    (* debug/info *)
    let _ : unit = Stdlib.at_exit (fun () ->
        Printf.printf "pcache op count: %#d (%s)\n" (!pcache_op_count) __FILE__)

    let raw_pcache_ops = pcache_ops

    let pcache_ops = 
      P.make_ops
        ~monad_ops
        ~pcache_ops:raw_pcache_ops
        ~pcache_blocks_limit
        ~bt_find
        ~bt_handle_detach

    let pcache_thread () = 
      let loop_evictees = 
        let rec loop es = 
          yield () >>= fun () ->
          (* Printf.printf "pcache_thread, loop_evictees\n%!"; *)
          match es with
          | [] -> return ()
          | kvop::es -> 
            Kvop.(match kvop with
            | Insert (k,v) -> 
              pcache_ops.insert k v >>= fun () ->
              loop es
            | Delete k -> 
              pcache_ops.delete k >>= fun () ->
              loop es)
        in
        loop
      in
      let rec read_and_dispatch () =
        (* FIXME do we need to yield if we are simply dequeueing? *)
        (* FIXME why is yield coerced to from_lwt? should be monad-agnostic *)
        yield () >>= fun () ->
        (* Printf.printf "pcache_thread read_and_dispatch starts\n%!"; *)
        mark mk3;
        q_lru_pc#dequeue () >>= fun msg ->
        mark (-1*mk3);
        (* Printf.printf "pcache_thread dequeued: %s\n%!" (Lru'.msg2string msg); *)
        (* FIXME the following pause seems to require that the btree
           thread makes progress, but of course it cannot since there
           are no msgs on the queue *)
        (* from_lwt(sleep pcache_thread_delay) >>= fun () ->  (\* FIXME *\) *)
        match msg with
        | Insert (k,v,callback) ->
          incr pcache_op_count;
          pcache_ops.insert k v >>= fun () -> 
          async (fun () -> callback ()) >>= fun () ->
          read_and_dispatch ()
        | Delete (k,callback) ->
          incr pcache_op_count;
          pcache_ops.delete k >>= fun () ->
          async (fun () -> callback ()) >>= fun () ->
          read_and_dispatch ()
        | Find (k,callback) -> 
          incr pcache_op_count;
          pcache_ops.find k >>= fun v ->
          async (fun () -> callback v) >>= fun () ->
          read_and_dispatch ()
        | Evictees es -> 
          pcache_op_count:=!pcache_op_count + List.length es;
          mark mk4;
          loop_evictees es >>= fun () ->
          read_and_dispatch ()
      in
      read_and_dispatch ()

    (** NOTE currently pcache doesn't sleep at all *)
                  
    let start_pcache_thread () : (unit,t)m = async (fun () -> pcache_thread ())
  end)
  in
  object 
    (* method pcache_ops=pcache_ops *)
    method start_pcache_thread=start_pcache_thread 
  end

  let _ = make_pcache_thread

end
