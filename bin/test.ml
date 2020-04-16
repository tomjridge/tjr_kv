(** Test the KV store with an LRU frontend *)
open Std_types
open Tjr_monad.With_lwt
let file_ops = lwt_file_ops

open Tjr_kv
open Intf_v2
open Lwt_aux

let rt_config = Lazy.force Kv_config_runtime.config

module KVX = Kv_store_with_lru.Int_int_ex

let i2k i = i
let i2v i = i

let Kv_config_runtime.{ tst_thrd_dly_its=dly_its; tst_thrd_dly; tst_thrd_yld_its; _ } = rt_config

(** Test thread runs a loop, inserting (i,2*i) at step i *)
let test_thread ~(q_lru_pc:(_,_)q_lru_pc) ~lru_ops = 
  let q_len = q_lru_pc#len in
  let maybe_yield n = 
    n mod tst_thrd_yld_its = 0 |> function 
    | true -> from_lwt(Lwt_aux.yield ())
    | false -> return ()
  in
  let maybe_sleep n =
    (* we allow the q_lru_pc to grow to this size without sleeping *)
    let cut_off = 500 in 
    begin
      (n mod dly_its = 0 && q_len () > cut_off) |> function
      | true -> 
        (* this results in somewhat jerky sleeping *)
        (* let len_sq = let x = q_len () in x*x in *)
        let len = q_len () - cut_off in
        let delta = rt_config.tst_thrd_dly *. float_of_int len in
        sleep delta |> from_lwt
      | false -> return ()
    end
  in
  let rec loop n =
    maybe_yield n >>= fun () -> 
    maybe_sleep n >>= fun () -> 
    let _ : unit = if n mod 10000 = 0 then Printf.printf "Inserting %#d\n%!" n in
    let mode = Persist_later in (* FIXME *)
    lru_ops.mt_insert mode (i2k n) (i2k(2*n)) >>= fun () ->
    loop (n+1)
  in
  object 
    method start_test_thread () : (unit,t)m = loop 0
  end

let [b1;b2;b3] = List.map B.of_int [1;2;3] [@@warning "-8"]

(** Start pcache, bt and test thread; wait 2s; then print some stats *)
let example = 
  lwt_file_ops.open_ ~fn:rt_config.filename ~create:true ~init:true >>= fun fd ->
  let blk_dev_ops = Blk_dev_factory.make_5 fd in
  KVX.blk_dev_ops_ref := Some(blk_dev_ops);  
  KVX.roots:=Some { bt_rt=b1; min_free=b3;pc_hd=b2;pc_tl=b2 };
  KVX.set_pcache_state (Pcache_intf.Pcache_state.empty_pcache_state ~root_ptr:b2 ~current_ptr:b2 ~empty:KVX.Pcache_.kvop_map_ops.empty); 

  let main_thread () = Lwt.(
      Lwt_unix.sleep 2.0 >>= fun () ->
      Printf.printf "Main thread terminating\n\n%!";
      Printf.printf "Queue sizes: q_lru_pc:%d; q_pc_bt:%d (%s)\n%!" 
        (KVX.q_lru_pc#len ())
        (KVX.q_pc_bt#len ())
        __FILE__ ;
      let roots = !KVX.roots |> Option.get in
      Printf.printf "B-tree root: %d (%s)\n%!" (roots.bt_rt |> B.to_int) __FILE__;
      Printf.printf "min_free blk_id: %d (%s)\n%!" (roots.min_free |> B.to_int) __FILE__;
      return ())
  in

  (* first we must write to the btree root block *)
  blk_dev_ops.write ~blk_id:b1 ~blk:(KVX.Btree_.empty_leaf_as_blk ()) >>= fun () ->
  Printf.printf "Wrote bt root blk\n%!";
  (* all threads *)
  Lwt.choose [
    to_lwt @@ (Lazy.force KVX.pcache_thread)#start_pcache_thread();
    to_lwt @@ (Lazy.force KVX.btree_thread)#start_btree_thread();
    to_lwt @@ (test_thread ~q_lru_pc:KVX.q_lru_pc ~lru_ops:KVX.lru_ops)#start_test_thread();
    main_thread ()
  ]
  |> from_lwt

let _ = Lwt_main.run (to_lwt example)
