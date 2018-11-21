(** An in-mem B-tree with sync, for testing. *)

open Tjr_monad.Types
open Btree_ops_type

module Ptr = Tjr_int.Make_type_isomorphic_to_int()

module Types = struct

  type bt_ptr = Ptr.t

  type ('k,'v) bt_map = ('k,'v) Tjr_polymap.t

  type ('k,'v) dummy_bt_state = {
    ptr: bt_ptr;  (* "current" pointer; revealed on next sync *)
    map: ('k,'v) bt_map;
    synced_states: (bt_ptr * ('k,'v) bt_map) list    
  }

end
include Types


let empty_btree () : ('k,'v) dummy_bt_state = 
  let map = Tjr_polymap.empty Pervasives.compare in
  let ptr = Ptr.int2t 0 in
  let synced_states = [] in
  { ptr; map; synced_states }

(* NOTE this assumes that ptr is greatest, which it should be given
   the implementation *)
let new_ptr state = state.ptr |> Ptr.t2int |> fun x -> x+1 |> Ptr.int2t


(** Simple implementation of the B-tree ops, for testing *)
let make_dummy_btree_ops 
    ~monad_ops ~with_state
  : ('k,'v,'blk_id,'t) btree_ops 
  = 
  let ( >>= ) = monad_ops.bind in
  let return = monad_ops.return in
  let with_btree = with_state.with_state in
  let find k = with_btree (fun ~state ~set_state -> 
      Tjr_polymap.find_opt k state.map |> return)
  in
  let insert k v = with_btree (fun ~state ~set_state -> 
      Tjr_polymap.add k v state.map |> fun map ->
      set_state {state with map})
  in
  let delete k = with_btree (fun ~state ~set_state -> 
      Tjr_polymap.remove k state.map |> fun map ->
      set_state {state with map})
  in
  let sync () = with_btree (fun ~state ~set_state ->
      let synced_states = (state.ptr,state.map)::state.synced_states in
      let ptr = new_ptr state in
      set_state {state with synced_states;ptr } >>= fun () ->
      return ptr)
  in
  {find;insert;delete;sync}

let _ = make_dummy_btree_ops
