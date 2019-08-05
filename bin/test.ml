(** Test the KV store with an LRU frontend *)

(* open Tjr_lru_cache.Persist_mode *)
(* open Util *)
open Tjr_monad.With_lwt
open Tjr_kv
open Lwt_aux
open Store_with_lru.Common_instances.Int_int

let test_config = Tjr_kv.Kv_config.config

let yield () = Lwt.return () (* Lwt_main.yield *)

let sleep f = Lwt_unix.sleep f

(* test thread ------------------------------------------------------ *)


let lru_ops = Lru'.lru_ops ()

(* let i2k i = string_of_int i *)
(* let i2v i = string_of_int i *)
let i2k i = i
let i2v i = i


let test_thread () = 
  let rec loop n = 
    let maybe_sleep = 
      if n mod test_config.test_thread_delay_iterations = 0 then
        from_lwt (sleep test_config.test_thread_delay)
      else return ()
    in
    (* need this yield so that sleeping thread gets a chance to run ? *)
    let maybe_yield = 
      if n mod test_config.test_thread_delay_iterations = 0 then 
        from_lwt(Lwt_main.yield ()) else return ()
    in
    maybe_sleep >>= fun () -> 
    maybe_yield >>= fun () -> 
    (if n mod 100000 = 0 then Printf.printf "Inserting %d\n%!" n else ());
    (* let mode = if n mod 100 = 0 then Persist_now else Persist_later in *)
    let mode = Persist_later in
    lru_ops.mt_insert mode (i2k n) (i2k(2*n)) >>= fun () ->
    loop (n+1)
  in
  loop 0


let _ =
  Lwt_main.run (Lwt.choose [
      to_lwt (Dmap'.dmap_thread ~yield ~sleep ());
      to_lwt (Btree'.btree_thread ~yield ~sleep ());
      to_lwt (test_thread());
      Lwt.(
        Lwt_unix.sleep 2.0 >>= fun () ->
        Printf.printf "Queue sizes: lru2dmap:%d; dmap2bt:%d\n%!" 
          (Queue.length q_lru_dmap_state.q)
          (Queue.length q_dmap_bt_state.q)
        ;
        let open Kv_profilers in
        if Tjr_kv.Kv_config.profiling_enabled then (
          Lru_profiler.print_summary (); 
          print_endline "";
          Dmap_profiler.print_summary (); 
          print_endline "";
          Bt_profiler.print_summary (); 
          return ())
        else return ()
      )])

