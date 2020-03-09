(** Lwt support FIXME move to shared? or tjr_monad? *)

open Tjr_monad.With_lwt
open Tjr_mem_queue.Memq_intf
(* open Kv_intf *)
(* open Intf_v2 *)

(* FIXME this isn't quite correct - we want lwt combined with state-passing of the fun_store *)

include struct

  let monad_ops = lwt_monad_ops

  let ( >>= ) = monad_ops.bind
  let return = monad_ops.return

  let event_ops = lwt_event_ops

end



(** async for lwt *)
let async : Tjr_monad.With_lwt.lwt Tjr_lru_cache.Mt_intf.Threading_types.async = 
  fun (f:unit -> (unit,lwt) m) : (unit,lwt) m ->
    Lwt.async (fun () -> f () |> to_lwt); return ()


(** {2 Lwt mutex basic funs} *)


(* FIXME move elsewhere - tjr_monad? *)

(* private; basic support from lwt. NOTE lock and wait are in Lwt.t *)
module Internal : sig
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
include Internal


(** {2 Lwt mutex ops, msg queue_ops} *)

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


(*

(** {2 In-memory message queue for Lwt} *)

(** Type for lwt_queue_ops *)
module Lwt_queue = struct
  type 'msg lwt_queue_ops = ('msg, (Lwt_mutex.t, unit Lwt_condition.t, 'msg) queue, lwt) memq_ops
end
open Lwt_queue

(** Construct queue ops *)
let queue_ops () = make_memq_ops ~monad_ops ~mutex_ops

let _ : unit -> ('a, (Lwt_mutex.t, unit Lwt_condition.t, 'a) queue, lwt) memq_ops = queue_ops


(* FIXME maybe avoid the unit arg *)
(** Construct empty queue *)
let empty_queue () = {
  q = Queue.create();
  mutex=create_mutex();
  cvar=create_cvar()
}

(** {2 Construct queues given k,v,blk_id} *)

module type S = sig
  type k
  type v
  type blk_id
end


(** This functor constructs [q_lru_pc] and [q_pc_bt], parameterized by k and v *)
module Make_queues(S:S) : sig
  (** Just an abbrev *)
  type 'a queue' = (Lwt_mutex.t, unit Lwt_condition.t, 'a) queue
  type lru_pc_msg' = (S.k, S.v, lwt) Msg_type.msg
  val q_lru_pc :
    (lru_pc_msg' queue', lru_pc_msg' lwt_queue_ops)
    initial_state_and_ops
  type pc_bt_msg' = (S.k, S.v, S.blk_id, lwt) Msg_pc_bt.pc_bt_msg
  val q_pc_bt :
    (pc_bt_msg' queue', pc_bt_msg' lwt_queue_ops)
    initial_state_and_ops
end
= struct
  open S

  type 'a queue' = (Lwt_mutex.t, unit Lwt_condition.t, 'a) queue

  (** {2 q_lru_pc}  *)

  type lru_pc_msg' = (k,v,lwt) Msg_lru_pc.lru_pc_msg
  module Internal2 = struct  
    let q_lru_pc : 
      (Lwt_mutex.t,unit Lwt_condition.t, lru_pc_msg') queue 
      = 
      (empty_queue ())
    let q_lru_pc_ops : lru_pc_msg' lwt_queue_ops = queue_ops ()
  end
  let q_lru_pc = {initial_state=Internal2.q_lru_pc;ops=Internal2.q_lru_pc_ops}

  (** {2 q_pc_bt } *)

  type pc_bt_msg' = (k,v,S.blk_id,lwt) Msg_pc_bt.pc_bt_msg
  module Internal3 = struct
    let q_pc_bt :
      (Lwt_mutex.t,unit Lwt_condition.t, pc_bt_msg') queue 
      = 
      (empty_queue ())
    let q_pc_bt_ops : pc_bt_msg' lwt_queue_ops = queue_ops ()
  end
  let q_pc_bt = {initial_state=Internal3.q_pc_bt; ops=Internal3.q_pc_bt_ops}
end
*)

let yield () = Lwt.pause ()

let sleep f = Lwt_unix.sleep f
