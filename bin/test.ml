(** Test the KV store with an LRU frontend *)

open Tjr_monad.With_lwt
let file_ops = lwt_file_ops

open Tjr_kv
open Lwt_aux

let test_config = Kv_runtime_config.config

module Blk_id = Blk_id_as_int
module B = Blk_id_as_int

type blk_id = Blk_id.blk_id[@@deriving bin_io]

type blk = ba_buf

module BTX = Tjr_btree_examples.Make_2.Int_int_ex

module PCX = Tjr_pcache_example.Int_int_ex

module KVX = Kv_store_with_lru.Int_int_ex



module Pvt_lwt = struct
  let yield () = Lwt.return () (* Lwt_main.yield *)

  let sleep f = Lwt_unix.sleep f
end
open Pvt_lwt


let lru_ops = KVX.Lru'.lru_ops ()


module Pvt = struct
  (* let i2k i = string_of_int i *)
  (* let i2v i = string_of_int i *)
  let i2k i = i
  let i2v i = i
end
open Pvt


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


(** Rt blk *)
module Rt_blk = struct
  (* open Bin_prot.Std *)

  (* This is what we want to write in the root block *)
  type rt_blk = {
    bt_rt         :blk_id;
    blk_alloc_ref :blk_id;
    pc_root       :blk_id;
    pc_current    :blk_id;
  } [@@deriving bin_io]

  class type manager = object
    (* val blk_dev_ops       : (blk_id,buf,lwt)blk_dev_ops *)
    method write_to_disk  : rt_blk -> (unit,lwt)m
    method read_from_disk : unit -> (rt_blk,lwt)m
  end
    
  let manager blk_dev_ops : manager = 
    let b0 = B.of_int 0 in
    let write_to_disk rt_blk =
      let buf = ba_buf_ops.create 4096 in
      let _ : int = bin_write_rt_blk buf ~pos:0 rt_blk in
      blk_dev_ops.write ~blk_id:b0 ~blk:buf
    in
    let read_from_disk () =
      blk_dev_ops.read ~blk_id:b0 >>= fun buf -> 
      bin_read_rt_blk buf ~pos_ref:(ref 0) |> return
    in
    object
      val blk_dev_ops=blk_dev_ops
      method write_to_disk = write_to_disk
      method read_from_disk = read_from_disk
    end
end

(** Runtime state (kvd is key-value descriptor) *)
type kvd = {
  fd               : Lwt_unix.file_descr;
  bt_rt_ref        : blk_id ref;
  blk_alloc_ref    : blk_id ref;
  pcache_state_ref : PCX.pcache_state ref
}

[@@@warning "-26"]

(** Start pcache, bt and test thread; wait 2s; then print some stats *)
let example = 

  let bt_rt = B.of_int 1 in
  let pc_rt = B.of_int 2 in
  let ba_min_free = B.of_int 3 in

  let bt_rt_ref = ref bt_rt in
  
  lwt_file_ops.open_ ~fn:test_config.bt_filename ~create:true ~init:true >>= fun fd ->

  let x = Blk_dev_factory.(make_7 fd) in
  let module X = (val x) in
  let open X in
  

  (* blk allocator *)
  let blk_alloc_ref = ref ba_min_free in
  let blk_alloc () = 
    let x = !blk_alloc_ref in
    B.incr blk_alloc_ref;
    return x
  in

  let kvd = {
    fd;
    bt_rt_ref=ref bt_rt;
    blk_alloc_ref;
    pcache_state_ref=ref 

  (* btree FIXME prefer to just return btree_ops? *)
  let bt_rt_ref = ref bt_rt in
  let btree = BTX.make ~blk_dev_ops ~blk_alloc ~root_ops
  let btree_thread = KVX.Btree'.btree_thread ~btree_ops ~yield ~sleep () in

  (* pcache *)
  let pc_state = Pcache_intf.Pcache_state.empty_pcache_state 
                   ~root_ptr:pc_rt ~current_ptr:pc_rt ~empty:PCX.kvop_map_ops.empty
  in
  let pcache_state_ref = ref pc_state in (* should share pc_root and pc_current *)
  let with_pcache = { with_state=(fun f -> 
      f ~state:!pcache_state_ref ~set_state:(fun s -> pcache_state_ref:=s; return ())) }
  in
  file_ops.open_ ~fn:test_config.pcache_filename ~create:true ~init:true >>= fun pc_fd ->
  let flush_tl (s:PCX.pcache_state) = file_ops.write_blk pc_fd (Blk_id.to_int s.current_ptr) s.buf in 
  (* FIXME shouldn't write_to_disk take a blk_dev_ops? *)
  PCX.make ~blk_alloc ~with_pcache ~flush_tl |> return >>= fun pcache_ops -> 
  let pcache_thread = KVX.Pcache'.pcache_thread ~pcache_ops ~yield ~sleep () in  

  (* now we can construct the runtime state *)
  let _kvd = {
    bt_fd;
    pc_fd;
    bt_rt_ref;
    blk_alloc_ref;
    pcache_state_ref
  } in

  let main_thread = Lwt.(
      Lwt_unix.sleep 2.0 >>= fun () ->
      Printf.printf "Queue sizes: lru2pcache:%d; pcache2bt:%d\n%!" 
        (Queue.length KVX.q_lru_pc_state.q)
        (Queue.length KVX.q_pc_bt_state.q);
      Printf.printf "B-tree root: %d\n" (!bt_rt_ref |> B.to_int);
      Printf.printf "Blk allocator state (min_free_blk_id): %d\n" (B.to_int !blk_alloc_ref);
      return ())
  in

  (* all threads *)
  Lwt.choose [
    to_lwt pcache_thread;
    to_lwt btree_thread;
    (to_lwt @@ test_thread ~q:KVX.q_lru_pc_state.q ());
    main_thread
  ]
  |> from_lwt

let _ = Lwt_main.run (to_lwt example)
