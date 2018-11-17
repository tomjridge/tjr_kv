(** Lwt support: Lwt combined with Tjr_store *)

(* open Lwt *)
(* open Blk_id_type *)

(**

We want the msg queue type to be a {! Tjr_store} ref.

*)

(** private; basic support from lwt. NOTE lock and wait are in Lwt.t *)
module A : sig
  val create_mutex : unit -> Lwt_mutex.t
  val create_cvar : unit -> 'a Lwt_condition.t
  val lock : Lwt_mutex.t -> unit Lwt.t
  val unlock : Lwt_mutex.t -> unit
  val signal : unit Lwt_condition.t -> unit
  val wait : mut:Lwt_mutex.t -> cvar:'a Lwt_condition.t -> 'a Lwt.t
end = struct 
  let create_mutex () = Lwt_mutex.create() 
  let create_cvar () = Lwt_condition.create() 
  let lock mut = Lwt_mutex.lock mut
  let unlock mut = Lwt_mutex.unlock mut
  let signal cvar = Lwt_condition.broadcast cvar ()
  let wait ~mut ~cvar = Lwt_condition.wait ~mutex:mut cvar
end
open A



(* In-memory message queue for Lwt ---------------------------------- *)

(** We construct the empty msg queue, and the [q_lru_dcl] reference to
   an empty message queue *)

open Tjr_mem_queue.Types

(* FIXME maybe avoid the unit arg *)
let empty () = {
  q = Queue.create();
  mutex=create_mutex();
  cvar=create_cvar()
}

(** private *)
module B = struct
  type ('k,'v,'t) msg' = ('k,'v,'t) Tjr_lru_cache.Msg_type.msg

  type lru_dcl_msg = (int,int,Tjr_store.t) msg'

  let q_lru_dcl : 
    (Lwt_mutex.t,unit Lwt_condition.t, lru_dcl_msg) queue Tjr_store.Refs.r 
    = 
    Fun_store.mk_ref' (empty ())
end
let q_lru_dcl = B.q_lru_dcl


(** private *)
module C = struct
  (* open Dcl_bt_msg_type *)
  type dcl_bt_msg = (int,int,Tjr_store.t) Dcl_bt_msg_type.dcl_bt_msg
  
  let q_dcl_bt :
    (Lwt_mutex.t,unit Lwt_condition.t, dcl_bt_msg) queue Tjr_store.Refs.r 
    = 
    Fun_store.mk_ref' (empty ())
  
end





(*
(* lwt monad ops ---------------------------------------------------- *)

(* use lwt *)

open Tjr_monad.Lwt_instance

let monad_ops = lwt_ops

let ( >>= ) = monad_ops.bind
let return = monad_ops.return

let event_ops = lwt_event_ops

module Lwt_ops = struct

  (* FIXME move mutex and cvar ops from mem_queue to tjr_monad; give
     lwt impl there *)

  open Tjr_mem_queue
  open Tjr_mem_queue.Types


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

  let queue_ops () = Mem_queue.make_ops ~monad_ops ~mutex_ops

end
include Lwt_ops

*)
