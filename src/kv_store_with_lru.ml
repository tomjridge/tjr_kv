(** A KV store with an LRU cache frontend. *)


(** {2 Architecture}

{%html: 

<a href="https://imgur.com/yQhDTnG"><img src="https://i.imgur.com/yQhDTnG.png" title="source: imgur.com" width='100%' /></a>


%}

We construct the following...



{3 LRU}

  - q_lru_pc, a msg queue from lru to pcache
  - Lru and lru_state
  - lru_thrd, which takes messages from lru.to_lower to enqueue on
    q_lru_pc; the lru interface itself can be used by many threads safely




{3 Pcache }

  - q_pc_bt, a msg queue from pcache to B-tree
  - Pcache and pcache_state
  - pc_thrd, which takes msgs from q_pc_btree and executes against dmap; also performs detach occasionally and enqueue messages to q_pc_bt


{3 B-tree}

  - B-tree
  - bt_thrd listening to q_pc_btree; also includes root manager functionality

*)

(* old doc?

{3 Functional store thread}

  - this maintains the "global" state, including locks etc
  - the queues are kept separate since they are implemented using
    mutation anyway FIXME perhaps prefer a "functional" version using
    the functional store
  - includes: lru_state; dmap_state; btree_state (a root pointer?)
  - also includes an Lwt_mvar for communication and implementation of
    with_state (?FIXME still true?)
*)

open Tjr_monad.With_lwt
open Kv_intf 
open Lwt_aux  (* provides various msg queues *)

open Kv_config
open Kv_profilers

module Blk_id = Blk_id_as_int

type blk_id = Blk_id.blk_id


(** We record the system state in a descriptor (kvd = "key-value descriptor") *)
type ('a,'b,'c,'d) kvd = {
  q_lru_pc_state:'a;
  q_pc_bt_state:'b;
  lru_state:'c;
  lru_ops:'d;
}


module type S = sig
  type k
  val compare: k -> k -> int
  type v

  type leaf_stream
end

module Make(S:S) = struct
  open S

  
  type kvop_map_carrier
  type kvop_map = (k,(k,v)kvop,kvop_map_carrier) Tjr_map.map

  (** {2 Message queues} *)
                    
  module Queues = Lwt_aux.Make_queues(struct include S type blk_id = Blk_id.blk_id end)
  open Queues

  (* NOTE queues are mutable *)
  let q_lru_pc_state = q_lru_pc.initial_state
  let q_pc_bt_state = q_pc_bt.initial_state



  (** {2 LRU cache} *)

  module Lru' : sig 
    val lru_ops : unit -> (k, v, lwt) mt_ops
  end = struct

    let [l2d_aa   ;l2d_ab] = 
      ["l2d:aa" ;"l2d:ab"] 
      |> List.map intern 
    [@@warning "-8"]
    let mark = lru_profiler.mark


    let from_lwt = With_lwt.from_lwt
    let to_lwt = With_lwt.to_lwt

    let lru_lock = Lwt_aux.create_mutex()

    module Lru_ = Tjr_lru_cache.Make(struct
        type k = S.k
        let compare = S.compare
        type v = S.v
        type t = lwt
        let monad_ops = monad_ops
        let async = async
        let event_ops = event_ops
      end)

    type mt_state = Lru_.mt_state

    let lru_state : mt_state ref = 
      Lru_.init_state
        ~max_size:lru_max_size
        ~evict_count:lru_evict_count
      |> ref  

    let enqueue msg = 
      return () >>= fun () ->
      mark l2d_aa;
      q_lru_pc.ops.memq_enqueue ~q:q_lru_pc_state ~msg >>= fun r -> 
      mark l2d_ab;
      return r

    let to_lower = enqueue  (* NOTE used in lru_callback_ops below FIXME
                               perhaps rename this type *)

    let with_lru (* : ('msg,'k,int,'t)with_lru_ops *) = 
      let with_lru f = 
        from_lwt (Lwt_mutex.lock lru_lock) >>= fun () ->
        f ~lru:(!lru_state) ~set_lru:(fun lru ->
          lru_state:=lru; return ()) >>= fun a ->
        (Lwt_mutex.unlock lru_lock; return a)
      in
      Mt_intf.Mt_state_type.{ with_lru }


    (* FIXME no need to shield this *)
    let lru_ops () = Lru_.make_multithreaded_lru ~with_lru ~to_lower

    let _ : unit -> (k,v,lwt) mt_ops = lru_ops

    let msg2string = 
      let open Msg_lru_pc in
      function
      | Insert(k,v,_) -> Printf.sprintf "Insert(%d,%d)" k v
      | Delete(k,_) -> Printf.sprintf "Delete(%d)" k
      | Find(k,_) -> Printf.sprintf "Find(%d)" k
      | Evictees es -> Printf.sprintf "Evictees(len=%d)" (List.length es)

  end
  let lru_ops = Lru'.lru_ops


  (** {2 Dmap and dmap_thread } *)

  module Dmap' : sig
    type nonrec dmap_ops = (k,v,blk_id,kvop_map,lwt) dmap_ops
    type nonrec dmap_state = (blk_id,kvop_map) Pcache_intf.Pvt.dmap_state
    val dmap_thread :
      dmap_ops :dmap_ops ->
      yield    :(unit -> unit Lwt.t) ->
      sleep    :(float -> unit Lwt.t) -> unit -> ('a, lwt) m
  end = struct
    type nonrec dmap_ops = (k,v,blk_id,kvop_map,lwt) dmap_ops
    type nonrec dmap_state = (blk_id,kvop_map) Pcache_intf.Pvt.dmap_state

    let [d2b_aa   ;d2b_ab   ;d2b_ca   ;d2b_cb   ;dmap_l2d_deq1   ;dmap_l2d_deq2   ;dmap_es] = 
      ["d2b:aa" ;"d2b:ab" ;"d2b:ca" ;"d2b:cb" ;"dmap:l2d.deq1" ;"dmap:l2d.deq2" ;"dmap_es"] 
      |> List.map intern
    [@@warning "-8"]
    let mark = dmap_profiler.mark

    (* open Dmap_types *)

    (** Now we fill in the missing components: [bt_find, bt_handle_detach].*)

    (** NOTE this enqueues a find event on the msg queue, and
        constructs a promise that waits for the result *)
    let bt_find = fun k ->
      event_ops.ev_create () >>= fun ev ->
      let callback = fun v -> event_ops.ev_signal ev v in
      mark d2b_aa; 
      q_pc_bt.ops.memq_enqueue
        ~q:q_pc_bt_state
        ~msg:Msg_pc_bt.(Find(k,callback)) >>= fun () ->
      mark d2b_ab; 
      event_ops.ev_wait ev

    let bt_handle_detach (detach_info:('k,'v,blk_id,'kvop_map)detach_info) =
      (* Printf.printf "bt_handle_detach start\n%!"; *)
      let kv_op_map = Kvop.default_kvop_map_ops () in
      let kv_ops = detach_info.past_map |> kv_op_map.bindings |> List.map snd in
      mark d2b_ca; 
      q_pc_bt.ops.memq_enqueue
        ~q:q_pc_bt_state
        ~msg:Msg_pc_bt.(Detach {
            ops=kv_ops;
            new_dmap_root=detach_info.current_ptr}) >>= fun _ ->
      mark d2b_cb; 
      return ()

    let _ = bt_handle_detach


    let dmap_op_count = ref 0
    let _ = Stdlib.at_exit (fun () ->
        Printf.printf "%s, pcache op count: %d\n" __MODULE__ (!dmap_op_count))

    let dmap_thread ~dmap_ops ~yield ~sleep () = 
      let dmap_ops = 
        let raw_dmap_ops = dmap_ops in
        Dmap_with_blocks_limit.make_ops
          ~monad_ops
          ~dmap_ops:raw_dmap_ops
          ~dmap_blocks_limit
          ~bt_find
          ~bt_handle_detach
      in
      let loop_evictees = 
        let rec loop es = 
          from_lwt(yield ()) >>= fun () ->
          (* Printf.printf "dmap_thread, loop_evictees\n%!"; *)
          match es with
          | [] -> return ()
          | (k,e)::es -> 
            (* let open Tjr_lru_cache in *)
            (* let open Mt_intf in *)
            match (e:v Tjr_lru_cache.entry) with
            | Insert { value=v; _ } -> 
              dmap_ops.insert k v >>= fun () ->
              loop es
            | Delete _ -> 
              dmap_ops.delete k >>= fun () ->
              loop es
            | Lower _ -> 
              Printf.printf "WARNING!!! unexpected evictee: Lower\n%!"; 
              assert(false) (* should never happen FIXME? *)
              (* loop es *)
              (* FIXME perhaps define a restricted type *)
        in
        loop
      in
      let rec read_and_dispatch () =
        (* FIXME do we need to yield if we are simply dequeueing? *)
        (* FIXME why is yield coerced to from_lwt? should be monad-agnostic *)
        from_lwt(yield ()) >>= fun () ->
        (* Printf.printf "dmap_thread read_and_dispatch starts\n%!"; *)
        mark dmap_l2d_deq1;
        q_lru_pc.ops.memq_dequeue q_lru_pc_state >>= fun msg ->
        mark dmap_l2d_deq2;
        (* Printf.printf "dmap_thread dequeued: %s\n%!" (Lru'.msg2string msg); *)
        (* FIXME the following pause seems to require that the btree
           thread makes progress, but of course it cannot since there
           are no msgs on the queue *)
        (* from_lwt(sleep dmap_thread_delay) >>= fun () ->  (\* FIXME *\) *)
        match msg with
        | Insert (k,v,callback) ->
          incr dmap_op_count;
          dmap_ops.insert k v >>= fun () -> 
          async (fun () -> callback ()) >>= fun () ->
          read_and_dispatch ()
        | Delete (k,callback) ->
          incr dmap_op_count;
          dmap_ops.delete k >>= fun () ->
          async (fun () -> callback ()) >>= fun () ->
          read_and_dispatch ()
        | Find (k,callback) -> 
          incr dmap_op_count;
          dmap_ops.find k >>= fun v ->
          async (fun () -> callback v) >>= fun () ->
          read_and_dispatch ()
        | Evictees es -> 
          dmap_op_count:=!dmap_op_count + List.length es;
          mark dmap_es;
          loop_evictees es >>= fun () ->
          read_and_dispatch ()
      in
      read_and_dispatch ()

  end
  let dmap_thread = Dmap'.dmap_thread

  (** {2 B-tree/btree ops/bt thread} *)

  module Btree' : sig
    val btree_thread :
      btree_ops:(k, v, blk_id, leaf_stream, lwt) map_ops_with_ls ->
      yield:(unit -> unit Lwt.t) ->
      sleep:(float -> unit Lwt.t) -> unit -> ('a, lwt) m
  end = struct
    (* open Btree_ops *)
    (* open Dummy_btree_implementation *)
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

    (** The thread listens at the end of the q_pc_btree for msgs which it
        then runs against the B-tree, and records the new root pair. *)
    let btree_thread ~btree_ops ~yield ~sleep () = 
      let module A = struct

        let bt_sync () = return (Blk_id_as_int.of_int (-1)) (* FIXME *)
          (* return ((failwith "FIXME"):Blk_id_as_int.blk_id) *)

        let Map_ops_with_ls.{ find; insert; delete; _ } = btree_ops

        let rec loop (ops:('k,'v)kvop list) = 
          (* from_lwt(yield()) >>= fun () -> *)  (* FIXME may want to yield occasionally *)
          match ops with
          | [] -> return ()
          | op::ops -> 
            incr btree_op_count;
            (* FIXME more efficient if we dealt with multiple ops eg insert_many *)
            (* NOTE the following do not have callbacks, because they come
               from a flush from the pcache (even if the LRU user
               requested sync... the sync write is to the pcache) *)
            match op with
            | Insert(k,v) -> 
              btree_ops.insert ~k ~v >>= fun () ->
              loop ops
            | Delete k -> 
              btree_ops.delete ~k >>= fun () ->
              loop ops

        let rec read_and_dispatch () =
          (* from_lwt(yield()) >>= fun () -> *)
          mark d2b_ea; 
          q_pc_bt.ops.memq_dequeue q_pc_bt_state >>= fun msg ->
          mark d2b_eb; 
          (* from_lwt(sleep bt_thread_delay) >>= fun () ->  (\* FIXME *\) *)
          (* Printf.printf "btree_thread dequeued: %s\n%!" "-"; *)
          match msg with
          | Find(k,callback) ->
            find ~k >>= fun v ->
            async(fun () -> callback v) >>= fun () ->
            read_and_dispatch ()
          | Detach { ops; new_dmap_root } ->
            loop ops >>= fun () ->
            (* FIXME what to do with the new root? maybe nothing for the
               time being? *)
            (* FIXME what about root pair? *)
            bt_sync () >>= fun ptr ->        
            Printf.printf 
              "New root pair: dmap_root=%d, bt_root=%d\n%!"
              (Blk_id.to_int new_dmap_root)
              (ptr |> Blk_id_as_int.to_int);
            read_and_dispatch ()
      end
      in
      A.read_and_dispatch ()
      
  end
  let btree_thread = Btree'.btree_thread

end


module Common_instances = struct

  module Int_int = struct

    type leaf_stream = Tjr_btree_examples.Examples.Int_int_ex.ls
    
    module Internal = struct
      type k = int
      let compare = Int_.compare
      type v = int
        
      type nonrec leaf_stream = leaf_stream
    end

    module Internal2 = Make(Internal)

    include Internal2
    
  end

end
