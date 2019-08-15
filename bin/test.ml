(** Test the KV store with an LRU frontend *)

open Tjr_monad.With_lwt
open Tjr_kv
open Lwt_aux
open Kv_store_with_lru.Common_instances.Int_int

let test_config = Tjr_kv.Kv_config.config

let yield () = Lwt.return () (* Lwt_main.yield *)

let sleep f = Lwt_unix.sleep f

let lru_ops = Lru'.lru_ops ()

(* let i2k i = string_of_int i *)
(* let i2v i = string_of_int i *)
let i2k i = i
let i2v i = i

(** Test thread runs a loop, inserting (i,2*i) at step i *)
let test_thread () = 
  let rec loop n = 
    let maybe_sleep = 
      if n mod test_config.test_thread_delay_iterations = 0 then
        from_lwt (sleep test_config.test_thread_delay)
      else return ()
    in
    (* need this yield so that sleeping thread gets a chance to run ? *)
    let maybe_yield = 
      if n mod test_config.test_thread_yield_iterations = 0 then 
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
    
(** Start dmap, bt and test thread; wait 2s; then print some stats *)
let _ =
  let module A = struct
    open Tjr_btree_examples 
    let example = Examples.Lwt.int_int_example () 
    let Examples.{monad_ops;blk_ops;empty_leaf_as_blk; blk_allocator_ref; btree_root_ref; _} = example 
    let ( >>= ) = monad_ops.bind
    let return = monad_ops.return 
    let Blk_layer.{ from_file; close } =
      Blk_layer.make_from_file_and_close ~monad_ops ~blk_ops
        ~empty_leaf_as_blk 

    (* let _ = blk_allocator_ref := {min_free_blk_id=Blk_id_as_int.of_int 2} *)
    (* let _ = btree_root_ref := {btree_root=Blk_id_as_int.of_int 1} *)

  end
  in
  let module B = struct
    open Lwt    
    let _ = 
      Lwt_main.run (
        to_lwt (A.from_file ~fn:test_config.bt_filename ~create:true ~init:true) 
        >>= fun (fd,ba_root,bt_root) -> 
        let fd = Lwt_unix.of_unix_file_descr fd in
        let example = Tjr_btree_examples.Examples.Lwt.int_int_example () in
        example.blk_allocator_ref := ba_root;
        example.btree_root_ref := bt_root;
        let btree_ops = example.map_ops_with_ls fd in
        Lwt.choose [
          to_lwt (Dmap'.dmap_thread ~yield ~sleep ());
          to_lwt (Btree'.btree_thread ~btree_ops ~yield ~sleep ());
          to_lwt (test_thread());
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
