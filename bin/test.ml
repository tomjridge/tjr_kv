(** Test the KV store with an LRU frontend *)
open Kv_intf
open Shared_ctxt
open Tjr_monad.With_lwt

let file_ops = lwt_file_ops

open Tjr_kv
(* open Lwt_aux *)

module KVX = Kv_store_with_lru.Int_int_ex

let i2k i = i
let i2v i = i

let Kv_config_runtime.{ 
    tst_thrd_dly_its=dly_its; tst_thrd_dly; tst_thrd_yld_its; 
    filename=fn; _ } 
  = Lazy.force Kv_config_runtime.config

(** Test thread runs a loop, inserting (i,2*i) at step i; q_lru_pc is
   used only to control the rate of inserts *)
let test_thread ~(q_lru_pc:(_,_,_)q_lru_pc) ~lru_ops = 
  let q_len = q_lru_pc#len in
  let maybe_yield n = 
    n mod tst_thrd_yld_its = 0 |> function 
    | true -> from_lwt(Tjr_monad.With_lwt.yield ())
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
        let delta = tst_thrd_dly *. float_of_int len in
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

(** Start pcache, bt and test thread; wait 2s; then print some stats *)
let example = 
  lwt_file_ops.open_ ~fn ~create:true ~init:true >>= fun fd ->
  let blk_dev_ops = Blk_dev_factory.make_5 fd in
  KVX.make ~blk_dev_ops ~init0:`Empty >>= fun kv_store ->
  let main_thread () = Lwt.(
      Lwt_unix.sleep 2.0 >>= fun () ->
      Printf.printf "Main thread terminating\n\n%!";
      Printf.printf "Queue sizes: q_lru_pc:%d; q_pc_bt:%d (%s)\n%!" 
        (kv_store#q_lru_pc#len ())
        (kv_store#q_pc_bt#len ())
        __FILE__ ;
      let roots = kv_store#rt_blk in
      Printf.printf "B-tree root: %d (%s)\n%!" (roots.bt_rt |> B.to_int) __FILE__;
      Printf.printf "min_free blk_id: %d (%s)\n%!" 
        (kv_store#min_free.min_free_blk_id |> B.to_int) __FILE__;
      return ())
  in
  (* all threads *)
  Lwt.choose [
    to_lwt @@ kv_store#pcache_thread#start_pcache_thread();
    to_lwt @@ kv_store#btree_thread#start_btree_thread();
    to_lwt @@ (test_thread ~q_lru_pc:kv_store#q_lru_pc ~lru_ops:kv_store#lru_ops)
              #start_test_thread();
    main_thread ()
  ]
  |> from_lwt

let _ = Lwt_main.run (to_lwt example)
