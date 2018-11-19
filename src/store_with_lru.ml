(** A KV store with the LRU frontend. *)

(** We construct:

- XXX Actually, work with references first XXX Functional store thread
  - this maintains the "global" state, including locks etc
  - the queues are kept separate since they are implemented using mutation anyway FIXME perhaps prefer a "functional" version using the functional store
  - includes:
    - lru_state
    - dcl_state
    - bt_state (a root pointer?)
  - also includes an Lwt_mvar for communication and implementation of with_state



- LRU
  - q_lru_dcl (msg queue from lru to dcl)
  - Lru (and lru_state)
  - Lru_t, which takes messages from lru.to_lower to enqueue on q_lru_dcl




- DCL
  - q_dcl_btree (msg queue from dcl to btree)
  - DCL (detachable chunked list) and dcl_state
  - DCL thread (Dcl_t)
    - takes msgs from q_dcl_btree and executes against dcl and B-tree


- B-tree
  - B-tree
  - B-tree thread (Btree_t) listening to q_dcl_btree
    - also includes root pair functionality



*)

open Tjr_monad.Types
open Tjr_mem_queue.Types


(* lwt ops ---------------------------------------------------- *)

(* see lwt_aux *)

open Lwt_aux

(* blk_id ----------------------------------------------------------- *)

open Blk_id_type



(* q_lru_dcl -------------------------------------------------------- *)

(* see lwt_aux, q_dcl_bt, ..._ops *)



(* lru -------------------------------------------------------------- *)


module Lru' = struct
  open Tjr_lru_cache.Multithreaded_lru

  let from_lwt = Tjr_monad.Lwt_instance.from_lwt
  let to_lwt = Tjr_monad.Lwt_instance.to_lwt
         
  let lru_lock = Lwt_aux.create_mutex()
  let lru_state : (int,int,lwt) lru_state ref = ref (
      Tjr_lru_cache.Mt_types.mt_initial_state 
        ~max_size:4 
        ~evict_count:2 
        ~compare_k:(fun (x:int) y -> Pervasives.compare x y))

  (** An empty message queue. NOTE not in funstore *)
  let q_lru_dcl = Lwt_aux.empty_queue ()
  
  let enqueue msg = q_lru_dcl_ops.enqueue ~q:q_lru_dcl ~msg

  let with_lru_ops : ('msg,'k,int,'t)with_lru_ops = 
    let with_lru f = 
      from_lwt (Lwt_mutex.lock lru_lock) >>= fun () ->
      f ~lru:(!lru_state) ~set_lru:(fun lru ->
          lru_state:=lru; return ()) >>= fun a ->
      (Lwt_mutex.unlock lru_lock; return a)
    in
    { with_lru }

  let async : Tjr_monad.Lwt_instance.lwt async = 
    fun (f:unit -> (unit,lwt) m) : (unit,lwt) m ->
      Lwt.async (fun () -> f () |> to_lwt); return ()

  let lru_callback_ops : (int,int,lwt) mt_callback_ops = make_lru_callback_ops ~monad_ops ~with_lru_ops ~async

  let _ = lru_callback_ops

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
      let int2t = fun i -> i 
      let t2int = fun ( i) -> i
    end
    module Pc_blk_id = struct 
      type t = blk_id
      let int2t = fun i ->  i 
      let t2int = fun ( i) -> i
    end
  end)




(* Q_dcl_btree ------------------------------------------------------ *)

(* see Lwt_aux *)



(* Dcl -------------------------------------------------------------- *)

(* FIXME we need a simple model of Dcl for testing... separate out
   from dcl.ml *)


(* Dcl_t ------------------------------------------------------------ *)



(* B-tree/btree ops ------------------------------------------------- *)

open Btree_ops

let btree_ops : (int,int,blk_id,lwt) btree_ops = 
  (* Btree_model.btree_ops ~monad_ops ~with_btree ~get_btree_root_and_incr *)
  failwith "FIXME"

(* FIXME what about root pair? *)



(* B-tree thread ---------------------------------------------------- *)

(** The thread listens at the end of the q_dcl_btree for msgs which it
   then runs against the B-tree, and records the new root pair. *)

module Btree_t = struct
  (* open Msg *)
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







    
    



(* old ================================================================ *)

(*

(* store ------------------------------------------------------------ *)



module Fun_store' = struct
  open Tjr_monad.Types
  open Fun_store

  (* FIXME probably not needed; used to enforce sequential access to
     the fun store *)
  let global_lock = create_mutex()


    
  let from_lwt = Tjr_monad.Lwt_instance.from_lwt

  type ('a,'b) mb_msg = 
    Set_lru of 'a
    | With_lru of 'b

  let mb : ('a,'b) mb_msg Lwt_mvar.t = Lwt_mvar.create_empty () 


  let fun_store_t ~async = 
    let rec loop state = 
      from_lwt (Lwt_mvar.take mb) >>= fun msg ->
      match msg with
      | Set_lru lru -> (
          set lru_state lru state |> loop)
      | With_lru kk -> (
          (* Because this is given to the calling thread, there needs
             to be a way to set the lru state from a non-funstore
             thread; so we just reuse the mbox; note that this relies
             on the call to set_lru being made asynchronously, so that
             the fun_store_t can service the set_lru request *)
          let set_lru lru = from_lwt (Lwt_mvar.put mb (Set_lru lru)) in
          from_lwt (Lwt_mutex.lock global_lock) >>= fun () ->
          from_lwt (Lwt_mutex.lock lru_lock) >>= fun () ->
          async(fun () ->
              (kk ~lru:(get lru_state state) ~set_lru:set_lru) >>= fun a ->
              (Lwt_mutex.unlock lru_lock; return ()));
          (Lwt_mutex.unlock global_lock; loop state))
    in
    fun state -> loop state

  let put msg = 
    from_lwt(Lwt_mvar.put msg mb)

  let _ = put

  

end
*)
