(** An uncached kv store *)

(** 

NOTE see diagram in <root>/docs/

At a high level, this is a k -> v map.

This is uncached: there is no cache in front of the store. The pcache
works on a raw uncached block device. The B-tree syncs the block
device after every batch operation.

We expect to use this with an LRU in front.

At intervals, when the pcache becomes long, some initial prefix of the
pcache is rolled into the B-tree. The root of the pcache is adjusted
(the old blocks can be reclaimed), and the root is written to disk
(but not necessarily synced). On crash, if the old root is used there
is no problem - we just replay these modifications over the B-tree. We
require that if the new pcache root hits disk, the B-tree is also on
disk. One approach is to async (flush btree; flush pcache root).

*)

open Tjr_monad.Types
(* open Tjr_pcache.Types *)
open Tjr_pcache.Ins_del_op_type

module type REQUIRES = sig 
  module Bt_blk_id:Tjr_int.TYPE_ISOMORPHIC_TO_INT
  module Pc_blk_id:Tjr_int.TYPE_ISOMORPHIC_TO_INT (* NOTE only needed for testing; otherwise abstract *)
end

module Make(Requires : REQUIRES) = struct
  
  open Requires
      
  open Tjr_btree.Map_ops

  open Tjr_pcache.Detachable_chunked_list


  type bt_blk_id = Bt_blk_id.t
  type pc_blk_id = Pc_blk_id.t


  (** We execute a roll up in another thread, to avoid blocking the
     pcache. The roll-up thread will typically finish with a new pair
     of roots. *)
  type root_pair = {
    pcache_root: pc_blk_id;
    btree_root: bt_blk_id;
  }


  (** This should execute in another dedicated rollup thread, so that
     the pcache thread is not blocked. We can't just instantiate this
     function here. In the pcache thread we need to join a msg to the
     end of a msg queue, signalling the rollup thread to perform a
     rollup. In this case, the rollup thread likely already knows
     operations such as [bt_insert] etc. which it can use to
     instantiate this function.

      NOTE that we return a unit so that from the pcache we can call
     this function without waiting for the rollup to occur. So the new
     roots have to be handled somewhere else (not in the pcache
     thread).  *)
  let execute_btree_rollup
    ~monad_ops
    ~(bt_insert:'k -> 'v -> (unit,'t) m)
    ~bt_delete
    ~bt_sync (* to sync the B-tree to get the new B-tree root *)
    ~(kvop_map_bindings:'op_map -> ('k,'v) op list)
    ~sync_new_roots  (* not clear if we should just return the new roots rather than explicitly passing in a sync op *)
    =
    let ( >>= ) = monad_ops.bind in
    let return = monad_ops.return in
    let f detach_result : (unit,'t) m =
      begin
        (* map consists of all the entries we need to roll up *)
        detach_result.old_map |> kvop_map_bindings |> fun ops ->
        let rec loop ops = 
          match ops with
          | [] -> return (`Finished(detach_result.old_ptr,detach_result.new_ptr))
          | v::ops ->
            match v with
            | Insert (k,v) -> bt_insert k v >>= fun () -> loop ops
            | Delete k -> bt_delete k >>= fun () -> loop ops
        in
        loop ops
      end
      >>= 
      begin
        function `Finished(old_root,(*new*)pcache_root) -> 
          (* sync the btree *)
          bt_sync () >>= fun btree_root ->
          (* now we need to flush the new roots to disk *)
          sync_new_roots {pcache_root; btree_root} >>= fun () ->
          return ()
      end
    in
    f


  type ('k,'v,'t) ukv_ops = ('k,'v,'t) Tjr_btree.Map_ops.map_ops

  (* we perform a "roll up" operation, merging the pcache into the
     B-tree, when the number of pcache blocks reaches
     pcache_blocks_limit *)

  (** Construct the UKV. Parameters:
- [monad_ops]
- [pcache_ops]
- [pcache_blocks_limit]: how many blocks in the pcache before attempting a roll-up; if the length of pcache is [>=] this limit, we attempt a roll-up; NOTE that this limit should be >= 2 (if we roll up with 1 block, then in fact nothing gets rolled up because we roll up "upto" the current block; not a problem but probably pointless for testing)
- [bt_find]: called if key not in pcache map  FIXME do we need a write-through cache here? or just rely on the front-end LRU? FIXME note that even if a rollup is taking place, we can use the old B-tree root for the [bt_find] operation.
- [execute_btree_rollup]: called to detach the rollup into another thread; typically this operation puts a msg on a message queue which is then received and acted upon by the dedicated rollup thread
  *)
  let make_ukv_ops
      ~monad_ops 
      ~pcache_ops 
      ~pcache_blocks_limit 
      ~bt_find
      ~execute_btree_rollup
    : ('k,'v,'t) ukv_ops 
    =
    (* let open Mref_plus in *)
    let ( >>= ) = monad_ops.bind in
    let return = monad_ops.return in
    let pc = pcache_ops in
    let find k = 
      pc.find k >>= fun op ->
      match op with
      | None -> bt_find k
      | Some op ->
        match op with
        | Insert(k,v) -> return (Some v)
        | Delete k -> return None
    in
    let maybe_roll_up () = 
      pc.undetached_block_count () >>= fun n ->
      match n >= pcache_blocks_limit with
      | false -> return `No_roll_up_needed
      | true -> 
        pc.detach () >>= fun detach_result ->
        execute_btree_rollup detach_result >>= fun () ->
        return `Ok
    in
    let insert k v =
      pc.add (Insert(k,v)) >>= fun () -> 
      maybe_roll_up () >>= fun _ ->
      return ()
    in
    let delete k =
      pc.add (Delete k) >>= fun () -> 
      maybe_roll_up () >>= fun _ ->
      return ()
    in
    let insert_many k v kvs = 
      (* FIXME we should do something smarter here *)
      insert k v >>= fun () -> return kvs
    in
    { find; insert; delete; insert_many }



end  (* Make *)
