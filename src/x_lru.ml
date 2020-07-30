(* NOTE this wasn't really doing much FIXME remove

(** Construct the LRU ops by joining the to the q_lru_pc *)

open Kv_intf
open Kv_config_profilers

module type S = sig
  type t
  val monad_ops : t monad_ops
  type k
  type v
  type lru
  val lru_factory   : (k,v,lru,t) lru_factory
end

module Make(S:S) = struct
  module S=S
  open S

  let ( >>= ) = monad_ops.bind
  let return = monad_ops.return

  let l2d_aa = intern "l2d:aa"
  let mark = lru_profiler.mark

  (* NOTE with_lru must be locked *)
  let make ~with_lru ~q_lru_pc = 
    let open (struct
    
      (* let lru_lock = mutex_ops.create_mutex() *)

      (* let _ = lru_lock *)

      let enqueue msg = 
        return () >>= fun () ->
        mark l2d_aa;
        q_lru_pc#enqueue msg >>= fun r -> 
        mark (-1 * l2d_aa);
        return r

      let to_lower = enqueue  (* NOTE used in lru_callback_ops below FIXME
                               perhaps rename this type *)

      let lru_ops = 
        lru_factory#make_ops ~with_state:with_lru ~to_lower 

    end)
    in
    lru_ops
end


(*
  type mutex 
  type cvar
  val mutex_ops : (mutex,cvar,t) mutex_ops
*)


(*
|> fun lru_ops -> 
        return @@ object 
          method lru_ops=lru_ops
          method lru_ref=lru_ref
        end
      let lru_ref = ref @@ lru_factory#empty 
          ~max_size:lru_params#max_size 
          ~evict_count:lru_params#evict_count
          

      let to_return = 
        with_locked_ref ~monad_ops ~mutex_ops lru_ref >>= fun with_state -> 
        lru_factory#make_ops ~with_state:with_state ~to_lower |> fun lru_ops -> 
        return @@ object 
          method lru_ops=lru_ops
          method lru_ref=lru_ref
        end
*)
*)
