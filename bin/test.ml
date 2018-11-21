(** Test the KV store with an LRU frontend *)

open Tjr_lru_cache.Persist_mode
open Tjr_kv
open Lwt_aux
open Store_with_lru

let yield = Lwt_main.yield

let test_thread () = 
  let rec loop n = 
    Printf.printf "Inserting %d\n%!" n;
    let mode = if n mod 100 = 0 then Persist_now else Persist_later in
    Lru'.lru_ops.insert mode n (2*n) >>= fun () ->
    from_lwt(yield ()) >>= fun () ->
    loop (n+1)
  in
  loop 0

let _ =
  Lwt_main.run (Lwt.join [
      to_lwt (Lru'.lru_thread ~yield);
      to_lwt (Dcl'.dcl_thread ~yield);
      to_lwt (Btree'.btree_thread ~yield);
      to_lwt (test_thread());
])

