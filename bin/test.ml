(** Test the KV store with an LRU frontend *)

open Tjr_lru_cache.Persist_mode
open Tjr_kv
open Lwt_aux
open Store_with_lru
open Tjr_profile


(* setup lru profiler ----------------------------------------------- *)

let mk_profiler () = 
  mk_profiler ~now:
    Core.Time_stamp_counter.(fun () ->
        now () |> to_int63 |> Core.Int63.to_int |> fun (Some x) -> x)
    [@@ocaml.warning "-8"]

let mt_profiler = mk_profiler ()

let _ = 
  Tjr_lru_cache.Multithreaded_lru.get_profiler_mark := (fun s ->
    assert(s="mt_mark");
    mt_profiler.mark)  

let lru_in_mem = mk_profiler ()
let get_evictees = mk_profiler ()

let _ = 
  Tjr_lru_cache.Lru_in_mem.get_profiler_mark := (fun s ->
      match s with
      | "lru_in_mem" -> lru_in_mem.mark
      | "get_evictees" -> get_evictees.mark
      | _ -> failwith __LOC__)

let profiler = 
  (* mt_profiler *)
(* lru_in_mem *)
  get_evictees

(* test thread ------------------------------------------------------ *)

let lru_ops = Lru'.lru_ops ()

let yield () = Lwt.return () (* Lwt_main.yield *)

let test_thread () = 
  let rec loop n = 
    (* need this yield so that sleeping thread gets a chance to run ? *)
    from_lwt(Lwt_main.yield ()) >>= fun () ->
    Printf.printf "Inserting %d\n%!" n;
    let mode = if n mod 100 = 0 then Persist_now else Persist_later in
    lru_ops.insert mode n (2*n) >>= fun () ->
    loop (n+1)
  in
  loop 0


let _ =
  Lwt_main.run (Lwt.choose [
      (* to_lwt (Lru'.lru_thread ~yield); *)
      to_lwt (Dcl'.dcl_thread ~yield);
      to_lwt (Btree'.btree_thread ~yield);
      to_lwt (test_thread());
      Lwt.(
        Lwt_unix.sleep 5.0 >>= fun () ->
        print_profile_summary (profiler.get_marks()); return ())
])


(* 
lru_in_mem
| Total time | wp1 | wp2 | count | Unit cost |
| 0 | fg | fg | 1 | 0 |
| 12259124 | cd | de | 119802 | 102 |
| 74462332 | ab | bc | 119802 | 621 |
| 143949920 | de | ef | 119802 | 1201 |
| 197733852 | bc | cd | 119802 | 1650 |
| 313721448 | ef | fg | 119802 | 2618 |  <- get_evictees
| 15221097676 | fg | ab | 119801 | 127053 |


get_evictees

| Total time | wp1 | wp2 | count | Unit cost |
| 0 | cd | cd | 1 | 0 |
| 157478144 | bc | cd | 7286 | 21613 |  
| 245059232 | ab | bc | 116625 | 2101 | <- cardinality computation, happens a lot
| 2852558196 | bc | ab | 109339 | 26089 |
| 12706343064 | cd | ab | 7285 | 1744178 |

| Total time | wp1 | wp2 | count | Unit cost |
| 0 | cd | cd | 1 | 0 |
| 18792636 | ab | bc | 116977 | 160 | <- fixed!
| 202384928 | bc | cd | 7308 | 27693 |
| 2927893144 | bc | ab | 109669 | 26697 |
| 12811861444 | cd | ab | 7307 | 1753368 |


*)
