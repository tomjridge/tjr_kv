(** A KV store with the LRU frontend. *)

(** We construct:

- lru
  - q_lru_dcl (msg queue from lru to dcl)
  - Lru
  - Lru_t, which takes messages from lru.to_lower to enqueue on q_lru_dcl

- synchronous store
  - q_dcl_btree (msg queue from dcl to btree)
  - DCL (detachable chunked list)
  - DCL thread (Dcl_t)
    - takes msgs from q_dcl_btree and executes against dcl and B-tree
  - B-tree
  - B-tree thread (Btree_t)
    - including root pair functionality


We connect:

- Lru -> Dcl_t with a message queue q_lru_dcl
- Dcl_t -> Btree_t with a message queue

*)

(* monad ops -------------------------------------------------------- *)

(* use lwt *)

open Tjr_monad.Lwt_instance

let monad_ops = lwt_ops

let ( >>= ) = monad_ops.bind
let return = monad_ops.return


(* other lwt ops ---------------------------------------------------- *)

let event_ops = lwt_event_ops

module Lwt_ops = struct

  (* FIXME move mutex and cvar ops from mem_queue to tjr_monad; give
     lwt impl there *)

  open Tjr_mem_queue.Mem_queue


  let mutex_ops : ('m,'c,'t) mutex_ops = {
    create_mutex=(fun () ->
        Lwt_mutex.create() |> fun mut ->
        return mut);
    create_cvar=(fun () ->
        Lwt_condition.create() |> fun cvar ->
        return cvar);
    lock=(fun mut ->
        Lwt_mutex.lock mut |> from_lwt);
    unlock=(fun mut ->
        Lwt_mutex.unlock mut |> fun _ -> return ());
    signal=(fun cvar ->
        Lwt_condition.broadcast cvar () |> fun _ -> return ());
    wait=(fun mut cvar ->
        Lwt_condition.wait ~mutex:mut cvar |> from_lwt)
  }

  let queue_ops () = make_ops ~monad_ops ~mutex_ops

end
include Lwt_ops



(* blk_id ----------------------------------------------------------- *)

type blk_id = Blk_id of int  



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



(* q_lru_dcl -------------------------------------------------------- *)

type ('k,'v,'t) msg = ('k,'v,'t) Tjr_lru_cache.Msg_type.msg

(* FIXME can we avoid this unit arg? try to get truly polymorphic ops *)
let q_lru_dcl_ops : ((int,int,Tjr_store.t) msg,'m,'c) Tjr_mem_queue.Mem_queue.queue_ops =
  queue_ops ()


(* lru -------------------------------------------------------------- *)


module Lru' = struct
  open Tjr_lru_cache.Lru_multithreaded

  let q_lru_dcl = failwith "FIXME"

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







    
    

