(** Test the KV store with an LRU frontend *)

(* open Tjr_lru_cache.Persist_mode *)
open Util
open Tjr_monad.With_lwt
open Tjr_kv
open Lwt_aux
open Store_with_lru

let yield () = Lwt.return () (* Lwt_main.yield *)

let sleep f = Lwt_unix.sleep f

open Config

let _ = lru_profiler := Tjr_profile.make_string_profiler ~now
let _ = dmap_profiler := Tjr_profile.make_string_profiler ~now
let _ = bt_profiler := Tjr_profile.make_string_profiler ~now

(* test thread ------------------------------------------------------ *)

let lru_ops = Lru'.lru_ops ()

let test_thread () = 
  let rec loop n = 
    let maybe_sleep = 
      if n mod config.test_thread_delay_iterations = 0 then
        from_lwt (sleep config.test_thread_delay)
      else return ()
    in
    (* need this yield so that sleeping thread gets a chance to run ? *)
    let maybe_yield = 
      if n mod config.test_thread_delay_iterations = 0 then 
        from_lwt(Lwt_main.yield ()) else return ()
    in
    maybe_sleep >>= fun () -> 
    maybe_yield >>= fun () -> 
    (if n mod 100000 = 0 then Printf.printf "Inserting %d\n%!" n else ());
    (* let mode = if n mod 100 = 0 then Persist_now else Persist_later in *)
    let mode = Persist_later in
    lru_ops.insert mode n (2*n) >>= fun () ->
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
          (Queue.length q_lru_dmap.q)
          (Queue.length q_dmap_bt.q)
        ;
        !lru_profiler.print_summary (); 
        print_endline "";
        print_endline "";
        !dmap_profiler.print_summary (); 
        print_endline "";
        print_endline "";
        !bt_profiler.print_summary (); 
        return ())
])

