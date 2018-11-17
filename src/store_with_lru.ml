(*
(** A KV store with the LRU frontend. *)

(** We construct:


- LRU
  - q_lru_dcl (msg queue from lru to dcl)
  - Lru
  - Lru_t, which takes messages from lru.to_lower to enqueue on q_lru_dcl




- DCL
  - q_dcl_btree (msg queue from dcl to btree)
  - DCL (detachable chunked list)
  - DCL thread (Dcl_t)
    - takes msgs from q_dcl_btree and executes against dcl and B-tree


- B-tree
  - B-tree
  - B-tree thread (Btree_t) listening to q_dcl_btree
    - also includes root pair functionality



*)

open Tjr_mem_queue.Types




(* lwt ops ---------------------------------------------------- *)

(* open Lwt_mutex_cvar_ops *)



(* blk_id ----------------------------------------------------------- *)

type blk_id = Blk_id of int  



(* q_lru_dcl -------------------------------------------------------- *)

type ('k,'v,'t) msg' = ('k,'v,'t) Tjr_lru_cache.Msg_type.msg

type lru_dcl_msg = (int,int,Tjr_store.t) msg'

(* FIXME can we avoid this unit arg? try to get truly polymorphic ops *)
(* FIXME why is the 'q type a queue? *)
let q_lru_dcl_ops : (lru_dcl_msg,'q,'t) queue_ops =
  queue_ops ()

let _ :
  (lru_dcl_msg, (Lwt_mutex.t, unit Lwt_condition.t, lru_dcl_msg) queue,
   Tjr_monad.Lwt_instance.lwt) queue_ops
= q_lru_dcl_ops



(* lru -------------------------------------------------------------- *)

module Lru' = struct
  open Tjr_lru_cache.Lru_multithreaded

  (** An empty message queue reference *)
  
  let q_lru_dcl = mk_ref' (Lwt_mq.empty ())

  (* This seems to require that q is the queue itself rather than some ref *)
  let enqueue msg = q_lru_dcl_ops.enqueue ~q:q_lru_dcl ~msg

  let with_lru_ops : ('msg,'k,'v,'t)with_lru_ops = failwith "FIXME"     

  let async = failwith "FIXME"

  (* FIXME add a type annot here *)
  let lru_callback_ops = make_lru_callback_ops ~monad_ops ~with_lru_ops ~async

  (* the interface we expose *)
  let lru_ops = failwith "FIXME"



  (* FIXME need to alter lru impl so that it takes an enqueue monadic
     operation as above; this uses the msg queue of course *)
  

end







(* sync store ------------------------------------------------------- *)

module Sync_store = Synchronous_store.Make(
  struct
    module Bt_blk_id = struct 
      type t = blk_id
      let int2t = fun i -> Blk_id i 
      let t2int = fun (Blk_id i) -> i
    end
    module Pc_blk_id = struct 
      type t = blk_id
      let int2t = fun i -> Blk_id i 
      let t2int = fun (Blk_id i) -> i
    end
  end)




(* Q_dcl_btree ------------------------------------------------------ *)

type 'map dcl_bt_msg = (blk_id,'map) Msg.dcl_bt_msg

let q_dcl_btree_ops = queue_ops ()



(* Dcl -------------------------------------------------------------- *)

(* FIXME we need a simple model of Dcl for testing... separate out
   from dcl.ml *)


(* Dcl_t ------------------------------------------------------------ *)



(* B-tree/btree ops ------------------------------------------------- *)

open Btree_ops

let btree_ops : ('k,'v,blk_id,'t) btree_ops = 
  (* Btree_model.btree_ops ~monad_ops ~with_btree ~get_btree_root_and_incr *)
  failwith "FIXME"

(* FIXME what about root pair? *)



(* B-tree thread ---------------------------------------------------- *)

(** The thread listens at the end of the q_dcl_btree for msgs which it
   then runs against the B-tree, and records the new root pair. *)

module Btree_t = struct
  open Msg
  open Tjr_mem_queue.Mem_queue

  let btree_t ~kvop_map_bindings ~sync_new_roots ~btree_ops ~q_ops ~q_dcl_btree = 
    let { find; insert=bt_insert; delete=bt_delete; sync=bt_sync } = btree_ops in
    let execute_btree_rollup = 
      Sync_store.execute_btree_rollup
        ~monad_ops
        ~bt_insert
        ~bt_delete
        ~bt_sync
        ~kvop_map_bindings
        ~sync_new_roots
    in
    let rec loop () =
      q_ops.dequeue ~q:q_dcl_btree >>= fun detach -> 
      execute_btree_rollup detach >>= fun () ->
      (* FIXME change ex to return new root *)
      loop ()
    in
    loop

  let _ = btree_t

end







    
    

*)
