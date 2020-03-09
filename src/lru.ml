(** The LRU *)

open Tjr_monad.With_lwt
open Lwt_aux
open Std_types
open Kv_intf
open Intf_v2
open Kv_profilers

module L = Tjr_lru_cache

(** args: make_multithreaded_lru, q_lru_pc *)
class type ['k,'v,'mt_state] args = object
  method make_multithreaded_lru: 
    with_lru:('mt_state, t) with_lru_ops ->
    to_lower:(('k, 'v, t) lru_pc_msg -> (unit, t) m) ->
    ('k, 'v, t)mt_ops
  method q_lru_pc: unit -> ('k,'v)q_lru_pc
end

(** result: get_lru_ops + ... *)
class type ['k,'v,'mt_state] lru = object
  method set_initial_state: 'mt_state -> unit
  (* method set_to_lower: (('k,'v,t)L.msg->(unit,t)m) -> unit *)
  method check_initialized: unit -> unit
  method get_lru_ops: unit -> ('k,'v,t)mt_ops
end

let make_lru (type k v mt_state) (args:(k,v,mt_state)args) 
  : (k,v,mt_state)lru = 
  let open (struct
    let [l2d_aa   ;l2d_ab] = 
      ["l2d:aa" ;"l2d:ab"] 
      |> List.map intern 
    [@@warning "-8"]
    let mark = lru_profiler.mark

    let q_lru_pc : (k,v) q_lru_pc = args#q_lru_pc ()

    let make_multithreaded_lru = args#make_multithreaded_lru

    let lru_lock = Lwt_aux.create_mutex()

    let enqueue msg = 
      return () >>= fun () ->
      mark l2d_aa;
      q_lru_pc#enqueue msg >>= fun r -> 
      mark l2d_ab;
      return r

    let to_lower = enqueue  (* NOTE used in lru_callback_ops below FIXME
                               perhaps rename this type *)

    let with_lru_ref = ref None

    let set_initial_state s = 
      let lru_state = ref s in
      let with_lru f = 
        from_lwt (Lwt_mutex.lock lru_lock) >>= fun () ->
        f ~lru:(!lru_state) ~set_lru:(fun lru ->
            lru_state:=lru; return ()) >>= fun a ->
        (Lwt_mutex.unlock lru_lock; return a)
      in
      with_lru_ref := Some (Mt_intf.Mt_state_type.{ with_lru })


    let check_initialized () = assert(Option.is_some !with_lru_ref)

    (* FIXME no need to shield this *)
    let lru_ops () = 
      check_initialized ();
      make_multithreaded_lru 
        ~with_lru:(!with_lru_ref |> Option.get) 
        ~to_lower

    let _ : unit -> (k,v,lwt) mt_ops = lru_ops

    let msg2string = 
      let open Msg_lru_pc in
      function
      | Insert(k,v,_) -> Printf.sprintf "Insert(%d,%d)" k v
      | Delete(k,_) -> Printf.sprintf "Delete(%d)" k
      | Find(k,_) -> Printf.sprintf "Find(%d)" k
      | Evictees es -> Printf.sprintf "Evictees(len=%d)" (List.length es)

  end)
  in
  object 
    method set_initial_state = set_initial_state
    method check_initialized = check_initialized
    method get_lru_ops = lru_ops
  end

let _ = make_lru
