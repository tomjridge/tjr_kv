(*
(** A B-tree, with imperative init, no cache, and access to q_pc_bt *)
class type ['k,'v,'ls] btree = object
  (* val bt_rt: blk_id ref *)
  method set_blk_dev_ops: std_blk_dev_ops -> unit
  method set_blk_allocator: std_blk_allocator_ops -> unit
  method set_with_bt_rt: (r,t) with_state -> unit
  method set_q_pc_bt: ('k,'v)q_pc_bt -> unit
  method check_initialized: unit -> unit
  method get_q_pc_bt: unit -> ('k,'v)q_pc_bt
  method get_map_ops: unit -> ('k,'v,r,'ls,t)map_ops_with_ls
  (* method get_ls_ops : unit -> ('k,'v,'ls,'t) ls_ops *)
end

let make_btree_1 ~make_btree : ('k,'v,'ls)btree = 
  let is_some,is_none = Option.(is_some,is_none) in
  let blk_dev_ops = ref None in
  let blk_allocator = ref None in
  let with_bt_rt = ref None in
  let q_pc_bt = ref None in
  let check_initialized () = 
    assert(is_some !blk_dev_ops);
    assert(is_some !blk_allocator);
    assert(is_some !with_bt_rt);
    assert(is_some !q_pc_bt);
    ()
  in
  object
    method set_blk_dev_ops x = blk_dev_ops:=Some x
    method set_blk_allocator x = blk_allocator:=Some x
    method set_with_bt_rt x = with_bt_rt:=Some x
    method set_q_pc_bt x = q_pc_bt:=Some x
    method check_initialized = check_initialized
    method get_q_pc_bt () = 
      assert(is_some !q_pc_bt);
      Option.get !q_pc_bt
    method get_map_ops () : ('k,'v,r,'ls,t) map_ops_with_ls = 
      check_initialized();
      make_btree ~blk_dev_ops:(Option.get !blk_dev_ops) ~blk_alloc:(Option.get !blk_allocator) ~root_ops:(Option.get !with_bt_rt)
  end

let _ = make_btree_1  
*)
