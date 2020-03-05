(** The uncached B-tree *)

open Tjr_monad.With_lwt
open Std_types
open Kv_intf
open Intf_v2
open Kv_profilers

module Pvt_lwt = struct
  let yield () = Lwt.pause ()

  let sleep f = Lwt_unix.sleep f
end
open Pvt_lwt

module type S = Tjr_btree_examples.Make_2.S

module Make(S:S) : sig
  open S
  type ls
  val make_btree: 
    blk_alloc:blk_alloc -> 
    q_pc_bt:(k,v)q_pc_bt ->
    bt_rt:blk_id -> 
    (k,v,ls) uncached_btree 
end
= struct
  open S
  module Btree = Tjr_btree_examples.Make_3.Make(S)


  type ls = Btree.ls

  open Kvop

  let [d2b_ea;d2b_eb] = 
    ["d2b_ea";"d2b_eb"] 
    |> List.map intern
  [@@warning "-8"]
  let mark = bt_profiler.mark

  open Msg_pc_bt

  let btree_op_count = ref 0

  let _ = Stdlib.at_exit (fun () ->
      Printf.printf "%s, B-tree op count: %d\n" __MODULE__ (!btree_op_count))

  (** The thread listens at the end of the q_pc_bt for msgs which it
      then runs against the B-tree, and records the new root pair. *)
  let make_btree ~(blk_alloc:blk_alloc) ~(q_pc_bt:(k,v)q_pc_bt) ~bt_rt : (k,v,ls)uncached_btree =
    let open (struct
      let bt_rt = ref bt_rt

      let with_bt_rt = with_ref bt_rt 

      let blk_dev_ops = blk_alloc#blk_dev_ops ()

      let btree = Btree.make ~blk_dev_ops ~blk_alloc:(blk_alloc#blk_allocator ()) ~root_ops:with_bt_rt

      let Map_ops_with_ls.{ find; insert; delete; _ } = btree#map_ops

      let bt_sync () = return (Blk_id_as_int.of_int (-1)) (* FIXME *)
      (* return ((failwith "FIXME"):Blk_id_as_int.blk_id) *)

      let rec loop (ops:('k,'v)kvop list) = 
        (* from_lwt(yield()) >>= fun () -> *)  
        (* FIXME may want to yield occasionally *)
        match ops with
        | [] -> return ()
        | op::ops -> 
          incr btree_op_count;
          (* FIXME more efficient if we dealt with multiple ops eg
             insert_many *)
          (* NOTE the following do not have callbacks, because they come
             from a flush from the pcache (even if the LRU user
             requested sync... the sync write is to the pcache) *)
          match op with
          | Insert(k,v) -> 
            insert ~k ~v >>= fun () ->
            loop ops
          | Delete k -> 
            delete ~k >>= fun () ->
            loop ops

      let rec read_and_dispatch () =
        (* from_lwt(yield()) >>= fun () -> *)
        mark d2b_ea; 
        (* FIXME are we worried about the cost of these dequeues? *)
        q_pc_bt#dequeue () >>= fun msg ->
        mark d2b_eb; 
        (* from_lwt(sleep bt_thread_delay) >>= fun () ->  (\* FIXME *\) *)
        (* Printf.printf "btree_thread dequeued: %s\n%!" "-"; *)
        match msg with
        | Find(k,callback) ->
          find ~k >>= fun v ->
          Lwt_aux.async(fun () -> callback v) >>= fun () ->
          read_and_dispatch ()
        | Detach { ops; new_pcache_root } ->
          loop ops >>= fun () ->
          (* FIXME what to do with the new root? maybe nothing for the
             time being? *)
          (* FIXME what about root pair? *)
          bt_sync () >>= fun ptr ->        
          Printf.printf 
            "New root pair: pcache_root=%d, bt_root=%d\n%!"
            (B.to_int new_pcache_root)
            (ptr |> B.to_int);
          read_and_dispatch ()
    end)
    in
    object
      val bt_rt = bt_rt
      method bt_rt = (fun () -> bt_rt)
      method start_btree_thread = read_and_dispatch
    end

end
