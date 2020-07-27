(** The B-tree worker thread *)

open Shared_ctxt
open Kv_intf
open Kv_config_profilers
open Root_manager



let make_btree_thread (type ls) 
    ~(write_origin:Origin.t -> (unit,t)m)  (* where we write the roots *)
    ~(q_pc_bt:(_,_,_)q_pc_bt)
    ~(map_ops:('k,'v,r,ls,t)map_ops_with_ls)
  : < start_btree_thread: unit -> (unit,t)m >
  = 
  let open (struct
    open Kvop

    let [d2b_ea;d2b_eb] = 
      ["d2b_ea";"d2b_eb"] 
      |> List.map intern
    [@@warning "-8"]
    let mark = bt_profiler.mark

    open Msg_pc_bt

    let btree_op_count = ref 0

    let _ : unit = Stdlib.at_exit (fun () ->
        Printf.printf "B-tree op count: %#d (%s)\n" (!btree_op_count) __FILE__)

    let Map_ops_with_ls.{ find; insert; delete; _ } = map_ops

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
      (* FIXME are we worried about the cost of these dequeues? most
         of the time they will pause *)
      q_pc_bt#dequeue () >>= fun msg ->
      mark (-1*d2b_ea); 
      (* from_lwt(sleep bt_thread_delay) >>= fun () ->  (\* FIXME *\) *)
      (* Printf.printf "btree_thread dequeued: %s\n%!" "-"; *)
      match msg with
      | Find(k,callback) ->
        find ~k >>= fun v ->
        async(fun () -> callback v) >>= fun () ->
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
        write_origin { pcache_root=new_pcache_root; btree_root=ptr } >>= fun () ->
        read_and_dispatch ()
  end)
  in
  object
    method start_btree_thread = read_and_dispatch
  end

let _ = make_btree_thread

