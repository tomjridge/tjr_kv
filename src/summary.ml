(** Summary *)

(** 

{2 Architecture}

{%html: 

<img src="https://docs.google.com/drawings/d/e/2PACX-1vSnTmJGnVDyxnrBZ_VOVZ7T0O9etqZa-BDPu-EPH9ziiNjY375TMgO-ENB9UO4e-HT3qmtbJKvFOFl0/pub?w=453&amp;h=373">

<img src="https://docs.google.com/drawings/d/e/2PACX-1vTIXhyNa7dovQYXuJXBMwPQZU99-x_tRdTIH3SkMUDyPwbL31zExWXauT2hO-eRIUcnGP3RVHiSHrjt/pub?w=557&amp;h=428">

%}
See {!Kv_store_with_lru} for more details


{2 Main interfaces} 

{[
module type S = sig
  type k[@@deriving bin_io]
  type v[@@deriving bin_io]
  val k_cmp: k -> k -> int
  type r = Shared_ctxt.r[@@deriving bin_io]

  val k_size: int
  val v_size: int
  val r_size: int
end
module type S' = sig
  (* NOTE specialized to shared_ctxt *)
  type k
  type v
  val k_cmp: k -> k -> int
  val k_mshlr: k bp_mshlr
  val v_mshlr: v bp_mshlr
end

type ('k,'v) kv_store = <
  blk_alloc     : (r, t) blk_allocator_ops;
  btree_thread  : < start_btree_thread : unit -> (unit, t)m >;
  lru_ops       : ('k, 'v, t) mt_ops;
  min_free      : min_free;
  pcache_thread : < start_pcache_thread : unit -> (unit, t)m >;
  q_lru_pc      : ('k, 'v, t) q_lru_pc;
  q_pc_bt       : ('k, 'v, t) q_pc_bt;
  root_man      : (rt_blk, t) root_man; 
  rt_blk        : rt_blk 
>

]}

*)
