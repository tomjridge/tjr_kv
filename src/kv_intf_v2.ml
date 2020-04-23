(** Interfaces based on classes *)

open Kv_intf
open Std_types

(** {2 Root manager and (uncached) B-tree} *)

class type ['a] generic_root_man = object
  method set_blk_dev_ops: std_blk_dev_ops -> unit
  method check_initialized: unit -> unit
  method read_roots: unit -> ('a,t)m
  method write_roots: 'a -> (unit,t)m
end

(** The particular instance we use *)

type rt_blk = {
  bt_rt:blk_id;
  min_free:blk_id;
  pc_hd:blk_id;
  pc_tl:blk_id
}

type root_man = rt_blk generic_root_man

(** NOTE the use of a unit arg to blk_dev_ops and others is to
   encourage that the blk_dev_ops method is used once only to avoid
   the overhead of repeatedly invoking object methods (the unit arg
   encourages thinking about when to pull out the blk_dev_ops) *)

class type blk_alloc = object
  val min_free: blk_id ref
  method blk_dev_ops: unit -> (blk_id,blk,t)blk_dev_ops (* put here so that we can pass blk_dev+allocator around easily *)
  method min_free: unit -> blk_id ref
  method blk_allocator: unit -> (r,t) blk_allocator_ops
end

type empty = |

class type ['k,'v,'ls] uncached_btree = object
  val bt_rt : blk_id ref
  method bt_rt: unit -> blk_id ref
  (* method map_ops: unit -> ('k,'v,r,'ls,t) map_ops_with_ls *)
  method start_btree_thread: unit -> (empty,t)m
  (* method with_bt_rt : unit -> (blk_id,t)with_state *)
end


(** {2 Pcache} *)

(** Messages from pcache to b-tree *)
type ('k,'v,'t) pc_bt_msg = ('k,'v,blk_id,'t) Msg_pc_bt.pc_bt_msg

class type ['k,'v] q_pc_bt = object
  method enqueue: ('k,'v,t) pc_bt_msg -> (unit,t)m
  method dequeue: unit -> (('k,'v,t) pc_bt_msg,t)m
  method len: unit -> int
end

(** NOTE the kvop_map type comes from the Pcache functor
   instantiation; perhaps it would be better to hide this type (it
   isn't used in msg_pc_bt - the bindings are converted to a list) *)
type 'kvop_map pcache_state = 
  (blk_id,'kvop_map)Pcache_intf.Pcache_state.pcache_state

class type ['k,'v,'kvop_map] pcache = object
  val pcache_state: 'kvop_map pcache_state ref
  method kvop_map_ops: unit -> ('k,'v,'kvop_map)Tjr_map.map_ops
  method pcache_state: unit -> 'kvop_map pcache_state ref
  method pcache_ops: unit -> ('k,'v,blk_id,'kvop_map,t) pcache_ops
end

(** {2 Lru} *)


(** Messages from lru to pcache *)
type ('k,'v,'t) lru_pc_msg = ('k,'v,'t) Msg_lru_pc.lru_pc_msg

class type ['k,'v] q_lru_pc = object
  method enqueue: ('k,'v,t) lru_pc_msg -> (unit,t)m
  method dequeue: unit -> (('k,'v,t) lru_pc_msg,t)m
  method len: unit -> int
end

class type ['k,'v,'mt_state] lru = object
  val lru_state: 'mt_state ref
  method lru_state: unit -> 'mt_state
  method lru_ops: unit -> ('k,'v,t)mt_ops
end
