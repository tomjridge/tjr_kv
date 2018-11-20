(** Lwt support: Lwt combined with Tjr_store; includes queue types and ops *)

include Tjr_monad.Lwt_instance
(* open Tjr_monad.Lwt_instance *)
open Tjr_mem_queue.Types


(* import lwt  ------------------------------------------------------ *)

(* this isn't quite correct - we want lwt combined with state-passing of the fun_store *)

module Lwt' = struct

  let monad_ops = lwt_ops

  let ( >>= ) = monad_ops.bind
  let return = monad_ops.return

  let event_ops = lwt_event_ops
end
include Lwt'

(* lwt mutex basic funs -------------------------------------------------- *)

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
include A


(* lwt mutex ops, queue_ops ------------------------------------------------ *)

module Lwt_mutex_ops = struct

  (* FIXME move mutex and cvar ops from mem_queue to tjr_monad; give
     lwt impl there *)

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

end
include Lwt_mutex_ops


(* In-memory message queue for Lwt ---------------------------------- *)

(** We construct the empty msg queue, and the [q_lru_dcl] reference to
   an empty message queue *)

module Lwt_queue = struct

  type 'msg lwt_queue_ops = ('msg, (Lwt_mutex.t, unit Lwt_condition.t, 'msg) queue, lwt) queue_ops

end
open Lwt_queue

let queue_ops () = 
  Tjr_mem_queue.Mem_queue.make_ops ~monad_ops ~mutex_ops

let _ = queue_ops


(* FIXME maybe avoid the unit arg *)
let empty_queue () = {
  q = Queue.create();
  mutex=create_mutex();
  cvar=create_cvar()
}


(* q_lru_dcl -------------------------------------------------------- *)

(** private *)
module B = struct

  type lru_dcl_msg' = (int,int,lwt) Lru_dcl_msg_type.lru_dcl_msg

  let q_lru_dcl : 
    (Lwt_mutex.t,unit Lwt_condition.t, lru_dcl_msg') queue 
    = 
    (empty_queue ())

  let q_lru_dcl_ops : lru_dcl_msg' lwt_queue_ops = queue_ops ()
end
let q_lru_dcl = B.q_lru_dcl
let q_lru_dcl_ops = B.q_lru_dcl_ops


(* q_dcl_bt --------------------------------------------------------- *)

(** private *)
module C = struct
  (* open Dcl_bt_msg_type *)
  type dcl_bt_msg' = (int,int,lwt) Dcl_bt_msg_type.dcl_bt_msg
  
  let q_dcl_bt :
    (Lwt_mutex.t,unit Lwt_condition.t, dcl_bt_msg') queue 
    = 
    (empty_queue ())
      
  let q_dcl_bt_ops : dcl_bt_msg' lwt_queue_ops = queue_ops ()
  
end
let q_dcl_bt = C.q_dcl_bt
let q_dcl_bt_ops = C.q_dcl_bt_ops 

