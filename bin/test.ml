(*

(** Test the KV store with an LRU frontend *)

open Tjr_monad.With_lwt
open Tjr_kv
open Lwt_aux
open Kv_store_with_lru.Common_instances.Int_int

module Blk_id = Blk_id_as_int
type blk_id = Blk_id.blk_id

let test_config = Tjr_kv.Kv_config.config

let yield () = Lwt.return () (* Lwt_main.yield *)

let sleep f = Lwt_unix.sleep f

let lru_ops = Lru'.lru_ops ()


module Ex = Tjr_pcache_example.Int_int_ex

module Runtime_state = struct
  type rs = {
    dmap_fd: Lwt_unix.file_descr;
    blk_aloc:(unit -> (blk_id,lwt)m);
  }


module Pcache_ = struct  

  let file_ops = lwt_file_ops

  let fn = test_config.dmap_filename 

  let dmap_ops ~blk_alloc ~with_dmap = 
    file_ops.open_ ~fn ~create:true ~init:true >>= fun fd ->
    (* FIXME shouldn't write_to_disk take a blk_dev_ops? *)
    let write_to_disk (s:Ex.dmap_state) = file_ops.write_blk fd (Blk_id.to_int s.current_ptr) s.buf in 
    Tjr_pcache_example.Int_int_ex.make ~blk_alloc ~with_dmap ~write_to_disk
end


(* let i2k i = string_of_int i *)
(* let i2v i = string_of_int i *)
let i2k i = i
let i2v i = i

(** Test thread runs a loop, inserting (i,2*i) at step i *)
let test_thread ~(q:'a Queue.t) () = 
  let _maybe_sleep n = 
    if n mod test_config.test_thread_delay_iterations = 0 then
      from_lwt (sleep test_config.test_thread_delay)
    else return ()
  in
  (* need this yield so that sleeping thread gets a chance to run ? *)
  let maybe_yield n = 
    if n mod test_config.test_thread_yield_iterations = 0 then 
      from_lwt(Lwt_main.yield ()) else return ()
  in
  let rec loop n =
    maybe_yield n >>= fun () -> 
    (let q_len = Queue.length q in
     let cond = n mod 1000 = 0 && q_len > 500 in
     match cond with
     | true -> from_lwt (sleep (test_config.test_thread_delay *. float_of_int (q_len - 500)))
     | false -> return ()) >>= fun () -> 
    (if n mod 100000 = 0 then Printf.printf "Inserting %d\n%!" n else ());
    (* let mode = if n mod 100 = 0 then Persist_now else Persist_later in *)
    let mode = Persist_later in
    lru_ops.mt_insert mode (i2k n) (i2k(2*n)) >>= fun () ->
    loop (n+1)
  in
  loop 0

(** Runtime state *)
module Kv_descr = struct

  type kvd = {
    bt_rt:blk_id ref;
    blk_alloc:blk_id ref;
  } [@@deriving bin_io]

end
open Kv_descr


(** Start dmap, bt and test thread; wait 2s; then print some stats *)
let _ =
  let module A = Tjr_btree_examples.Make_example.Pvt.Make_1(S_int_int) in
(*
struct
    open Tjr_btree_examples 
    let example = Examples.Int_int_exampleLwt.int_int_example () 
    let Examples.{monad_ops;blk_ops;empty_leaf_as_blk; blk_allocator_ref; btree_root_ref; _} = example 
    let ( >>= ) = monad_ops.bind
    let return = monad_ops.return 
    let { from_file; close } =
      Blk_layer_2.make_from_file_and_close ~monad_ops ~blk_ops
        ~empty_leaf_as_blk 

    (* let _ = blk_allocator_ref := {min_free_blk_id=Blk_id_as_int.of_int 2} *)
    (* let _ = btree_root_ref := {btree_root=Blk_id_as_int.of_int 1} *)

  end
  in
*)
  let module B = struct
    open Lwt    
    let _ = 
      Lwt_main.run (
        to_lwt Pcache_.dmap_ops >>= fun dmap_ops ->
        to_lwt (lwt_file_ops.open_ ~fn:test_config.bt_filename ~create:true ~init:true)
        >>= fun fd ->
        (* (fd,ba_root,bt_root) ->  *)
        let fd = Lwt_unix.of_unix_file_descr fd in
        let example = Tjr_btree_examples.Examples.Lwt.int_int_example () in
        example.blk_allocator_ref := ba_root;
        example.btree_root_ref := bt_root;
        let btree_ops = example.map_ops_with_ls ~note_cached:() fd in
        Lwt.choose [
          to_lwt (Dmap'.dmap_thread ~dmap_ops ~yield ~sleep ());
          to_lwt (Btree'.btree_thread ~btree_ops ~yield ~sleep ());
          to_lwt (test_thread ~q:q_lru_dmap_state.q ());
          Lwt.(
            Lwt_unix.sleep 2.0 >>= fun () ->
            Printf.printf "Queue sizes: lru2dmap:%d; dmap2bt:%d\n%!" 
              (Queue.length q_lru_dmap_state.q)
              (Queue.length q_dmap_bt_state.q);
            let btree_root = (!(example.btree_root_ref)).btree_root |> Blk_id_as_int.to_int in
            Printf.printf "B-tree root: %d\n" btree_root;
            let ba_root = (!(example.blk_allocator_ref)).min_free_blk_id |> Blk_id_as_int.to_int in
            Printf.printf "Blk allocator state (min_free_blk_id): %d\n" ba_root;
            return ()
          )])
  end
  in
  ()
*)
