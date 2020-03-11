(** Pcache worker thread *)

open Tjr_monad.With_lwt
open Lwt_aux
open Std_types
open Kv_intf
open Intf_v2
open Kv_conf_profilers

let make_pcache_thread (type k v ls kvop_map)
    ~(kvop_map_ops:(k,(k,v)kvop,kvop_map)Tjr_map.map_ops)
    ~pcache_blocks_limit
    ~(pcache_ops:(_,_,_,_,_)pcache_ops)
    ~(q_lru_pc:(_,_)q_lru_pc)
    ~(q_pc_bt:(_,_)q_pc_bt)
  : < start_pcache_thread: unit -> (unit,t)m > 
  =
  let open (struct

    type nonrec pcache_ops = (k,v,blk_id,kvop_map,lwt) pcache_ops
    type nonrec pcache_state = (blk_id,kvop_map) Pcache_intf.Pcache_state.pcache_state

    let [d2b_aa   ;d2b_ab   ;d2b_ca   ;d2b_cb   ;pcache_l2d_deq1   ;pcache_l2d_deq2   ;pcache_es] = 
      ["d2b:aa" ;"d2b:ab" ;"d2b:ca" ;"d2b:cb" ;"pcache:l2d.deq1" ;"pcache:l2d.deq2" ;"pcache_es"] 
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
      mark d2b_aa; 
      q_pc_bt#enqueue (Find(k,callback)) >>= fun () ->
      mark d2b_ab; 
      event_ops.ev_wait ev

    let bt_handle_detach (detach_info:('k,'v,blk_id,'kvop_map)detach_info) =
      (* Printf.printf "bt_handle_detach start\n%!"; *)
      let kv_ops = detach_info.past_map |> kvop_map_ops.bindings |> List.map snd in
      mark d2b_ca; 
      q_pc_bt#enqueue
        Msg_pc_bt.(Detach {
            ops=kv_ops;
            new_pcache_root=detach_info.current_ptr}) >>= fun _ ->
      mark d2b_cb; 
      return ()

    let _ = bt_handle_detach


    let pcache_op_count = ref 0
    let _ : unit = Stdlib.at_exit (fun () ->
        Printf.printf "pcache op count: %d (%s)\n" (!pcache_op_count) __FILE__)

    let pcache_thread ~pcache_ops ~yield ~sleep () = 
      let pcache_ops = 
        let raw_pcache_ops = pcache_ops in
        Pcache_with_blocks_limit.make_ops
          ~monad_ops
          ~pcache_ops:raw_pcache_ops
          ~pcache_blocks_limit
          ~bt_find
          ~bt_handle_detach
      in
      let loop_evictees = 
        let rec loop es = 
          from_lwt(yield ()) >>= fun () ->
          (* Printf.printf "pcache_thread, loop_evictees\n%!"; *)
          match es with
          | [] -> return ()
          | (k,e)::es -> 
            (* let open Tjr_lru_cache in *)
            (* let open Mt_intf in *)
            match (e:v Tjr_lru_cache.Im_intf.entry) with
            | Insert { value=v; _ } -> 
              pcache_ops.insert k v >>= fun () ->
              loop es
            | Delete _ -> 
              pcache_ops.delete k >>= fun () ->
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
        (* Printf.printf "pcache_thread read_and_dispatch starts\n%!"; *)
        mark pcache_l2d_deq1;
        q_lru_pc#dequeue () >>= fun msg ->
        mark pcache_l2d_deq2;
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
          mark pcache_es;
          loop_evictees es >>= fun () ->
          read_and_dispatch ()
      in
      read_and_dispatch ()

    let start_pcache_thread () : (unit,t)m = Lwt_aux.(pcache_thread ~pcache_ops ~yield ~sleep ())
  end)
  in
  object method start_pcache_thread=start_pcache_thread end

let _ = make_pcache_thread
