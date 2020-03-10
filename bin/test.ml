(** Test the KV store with an LRU frontend *)
open Std_types
open Tjr_monad.With_lwt
let file_ops = lwt_file_ops

open Tjr_kv
open Intf_v2
open Lwt_aux

let test_config = Kv_runtime_config.config

module KVX = Kv_store_with_lru.Int_int_ex

module Pvt = struct
  (* let i2k i = string_of_int i *)
  (* let i2v i = string_of_int i *)
  let i2k i = i
  let i2v i = i
end
open Pvt


(** Test thread runs a loop, inserting (i,2*i) at step i *)
let test_thread ~(q_lru_pc:(_,_)q_lru_pc) ~lru_ops = 
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
    (let q_len = q_lru_pc#len () in
     let cond = n mod 1000 = 0 && q_len > 500 in
     match cond with
     | true -> from_lwt (sleep (test_config.test_thread_delay *. float_of_int (q_len - 500)))
     | false -> return ()) >>= fun () -> 
    (if true (* n mod 10 = 0 *) then Printf.printf "Inserting %d\n%!" n else ());
    (* let mode = if n mod 100 = 0 then Persist_now else Persist_later in *)
    let mode = Persist_later in
    lru_ops.mt_insert mode (i2k n) (i2k(2*n)) >>= fun () ->
    loop (n+1)
  in
  object 
    method start_test_thread () = loop 0
  end

let [b1;b2;b3] = List.map B.of_int [1;2;3] [@@warning "-8"]

(** Start pcache, bt and test thread; wait 2s; then print some stats *)
let example = 

  lwt_file_ops.open_ ~fn:test_config.bt_filename ~create:true ~init:true >>= fun fd ->
  let blk_dev_ops = Blk_dev_factory.make_5 fd in
  KVX.blk_dev_ops_ref := Some(blk_dev_ops);  
  KVX.roots:=Some { bt_rt=b1; min_free=b3;pc_hd=b2;pc_tl=b2 };
  KVX.set_pcache_state (Pcache_intf.Pcache_state.empty_pcache_state ~root_ptr:b2 ~current_ptr:b2 ~empty:KVX.Pcache_.kvop_map_ops.empty); 

  let main_thread () = Lwt.(
      Lwt_unix.sleep 2.0 >>= fun () ->
      Printf.printf "Queue sizes: lru2pcache:%d; pcache2bt:%d\n%!" 
        (KVX.q_lru_pc#len ())
        (KVX.q_pc_bt#len ());
      let roots = !KVX.roots |> Option.get in
      Printf.printf "B-tree root: %d\n%!" (roots.bt_rt |> B.to_int);
      Printf.printf "Blk allocator state (min_free_blk_id): %d\n%!" (roots.min_free |> B.to_int);
      return ())
  in

  (* first we must write to the btree root block *)
  blk_dev_ops.write ~blk_id:b1 ~blk:(KVX.Btree_.empty_leaf_as_blk ()) >>= fun () ->
  Printf.printf "Wrote bt root blk\n%!";
  (* all threads *)
  Lwt.choose [
    to_lwt (Lazy.force KVX.pcache_thread |> fun x -> x#start_pcache_thread()>>= fun _ -> return ());
    to_lwt (Lazy.force KVX.btree_thread |> fun x -> x#start_btree_thread() >>= fun _ -> return ());
    to_lwt ((test_thread ~q_lru_pc:KVX.q_lru_pc ~lru_ops:KVX.lru_ops)#start_test_thread() >>= fun _ -> return ());
    main_thread ()
  ]
  |> from_lwt

let _ = Lwt_main.run (to_lwt example)
