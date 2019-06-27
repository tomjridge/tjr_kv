module Blk_id_as_int = struct
  type blk_id = int
end

module Btree_ops = struct
  (* FIXME also include "batch" op *)

  (** The operations supported by the B-tree. Even though the B-tree is
      synchronous, the "sync" operation is used to reveal the current
      root block. *)
  type ('k,'v,'blkid,'t) btree_ops = {
    find: 'k -> ('v option,'t)m;
    insert: 'k -> 'v -> (unit,'t)m;
    delete: 'k -> (unit,'t)m;
    sync: unit -> ('blkid,'t)m;  
  }

end

module Msg_dmap_bt = struct
  (** The type of messages sent from the DCL to the B-tree.

      This is a callback-oriented interface, with operations [find] and
      [detach] (to handle a list of operations... assumed to come from a
      detach operation via a map ie no duplicate keys)

  *)

  open Ins_del_op  (* FIXME move to fs_shared *)
  open Blk_id_as_int

  type ('k,'v,'t) dmap_bt_msg = 
    | Find of 'k * ('v option -> (unit,'t) m)
    | Detach of {
        ops: ('k,'v) op list;
        new_dmap_root: blk_id
      }
end

module Msg_lru_dmap = struct

  type ('k,'v,'t) lru_dmap_msg
    = ('k,'v,'t) Tjr_lru_cache.Mt_intf.msg
    =  Insert of 'k*'v*(unit -> (unit,'t)m)
    | Delete of 'k*(unit -> (unit,'t)m)
    | Find of 'k * ('v option -> (unit,'t)m)
    | Evictees of ('k * 'v Tjr_lru_cache.Im_intf.entry) list

end



module Pmap = struct
  (** A persistent map is formed from a B-tree and a detachable map
      (dmap).

      The "root" is a pair of the root for the persistent map and the root
      for the B-tree.

      When a pmap is constructed, we expect that the implementation requires
      some way to handle the dmap detach event followed by the B-tree
      rollup and the creation of the new root pair.

  *)

  type ('k,'v,'t) pmap = {
    find: 'k -> ('v option,'t) m;
    insert: 'k -> 'v -> (unit,'t) m;
    delete: 'k -> (unit,'t)m;
  }
end


module Syncable_map = struct



  (** A syncable map API.

      Sync operations:

      - sync to sync the whole map to disk
      - ksync to sync a particular key
      - ks_sync to sync a set of keys

  *)
  type ('k,'v,'ptr,'t) syncable_map = {
    find: 'k -> ('v option,'t)m;
    insert: 'k -> 'v -> (unit,'t)m;
    delete: 'k -> (unit,'t)m;
    sync: unit -> ('ptr,'t)m;
    ksync: 'k -> ('ptr,'t)m;
    ks_sync: 'k list -> ('ptr,'t)m
  }
end

