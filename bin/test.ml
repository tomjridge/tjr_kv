(** Test the KV store with an LRU frontend *)

open Tjr_lru_cache.Persist_mode
open Tjr_kv
open Lwt_aux
open Store_with_lru
open Tjr_profile


(* setup lru profiler ----------------------------------------------- *)

let _ =
  Profile_manager.now := 
    Core.Time_stamp_counter.(fun () ->
        now () |> to_int63 |> Core.Int63.to_int |> fun (Some x) -> x)
[@@ocaml.warning "-8"]

let profiler = Profile_manager.create_profiler ~name:"lru_in_mem.perform"

(* test thread ------------------------------------------------------ *)

let lru_ops = Lru'.lru_ops ()

let yield () = Lwt.return () (* Lwt_main.yield *)

let test_thread () = 
  let rec loop n = 
    (* need this yield so that sleeping thread gets a chance to run ? *)
    (if true (* n mod 127 = 0 *) then from_lwt(Lwt_main.yield ()) else return ()) >>= fun () ->
    (if n mod 1000 = 0 then Printf.printf "Inserting %d\n%!" n else ());
    (* let mode = if n mod 100 = 0 then Persist_now else Persist_later in *)
    let mode = Persist_later in
    lru_ops.insert mode n (2*n) >>= fun () ->
    loop (n+1)
  in
  loop 0


let _ =
  Lwt_main.run (Lwt.choose [
      to_lwt (Dcl'.dcl_thread ~yield);
      to_lwt (Btree'.btree_thread ~yield);
      to_lwt (test_thread());
      Lwt.(
        Lwt_unix.sleep 5.0 >>= fun () ->
        print_profile_summary (profiler.get_marks()); return ())
])


(* 

* lru_in_mem

|  Total time | wp1 | wp2 |  count | Unit cost |          |
|           0 | fg  | fg  |      1 |         0 |          |
|   240929480 | cd  | de  | 552116 |       436 |          |
|   591895828 |     | bc  | 552116 |      1072 |          |
|   927321220 | de  | ef  | 552116 |      1679 |          |
|  1049305288 | bc  | cd  | 552116 |      1900 | <- (ADD) |
|  1549011256 | ef  | fg  | 552116 |      2805 | <- (GE)  |
| 11604368580 | fg  |     | 552115 |     21018 |          |

- ADD: add new entry to map; presumably fairly optimal    
- GE: get_evictees; but mostly this is a noop; so there are some calls
  which are very expensive; even so, if this was cost 0, the overall
  time would only be halved for lru_in_mem


* get_evictees

|  Total time | wp1 | wp2 |  count | Unit cost |                             |
|           0 | bc  | bc  |      1 |         0 |                             |
|    32062004 | ab  | bc  | 562020 |        57 |                             |
|   351269668 |     | ab  | 562020 |       625 |                             |
|  1094563400 | bc  | cd  |  35123 |     31163 | <- most of the cost is here |
|  1140293264 | cd  |     |  35123 |     32465 |                             |
| 13345048584 | bc  |     | 526896 |     25327 |                             |


* timings

- 121953 inserts in 5 s = (/ 121953 5) = 24390/s
- 552116 inserts in 5s, with only the test thread
- 122561 inserts in 5s, with dcl thread, but no btree thread
  - so the bt thread is mostly irrelevant to performance

*)
