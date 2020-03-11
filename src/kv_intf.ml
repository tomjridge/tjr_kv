(*
module Blk_id_as_int = struct
  type blk_id = int
end
*)

module Btree_ops = struct
  (* FIXME also include "batch" op *)

  (** The operations supported by the B-tree. Even though the B-tree is
      synchronous, the "sync" operation is used to reveal the current
      root block. *)
  type ('k,'v,'blk_id,'t) btree_ops = {
    find: 'k -> ('v option,'t)m;
    insert: 'k -> 'v -> (unit,'t)m;
    delete: 'k -> (unit,'t)m;
    sync: unit -> ('blk_id,'t)m;  
  }

end

module Msg_pc_bt = struct
  (** The type of messages sent from the DCL to the B-tree.

      This is a callback-oriented interface, with operations [find] and
      [detach] (to handle a list of operations... assumed to come from a
      detach operation via a map ie no duplicate keys)

  *)

  open Kvop
  (* open Blk_id_as_int *)

  type ('k,'v,'blk_id,'t) pc_bt_msg = 
    | Find of 'k * ('v option -> (unit,'t) m)
    | Detach of {
        ops: ('k,'v) kvop list;
        new_pcache_root: 'blk_id
      }
end

module Msg_lru_pc = struct
  open Im_intf
  (* FIXME this should probably be moved to fs_shared *)
  type ('k,'v,'t) lru_pc_msg
    = ('k,'v,'t) Tjr_lru_cache.Mt_intf.Msg_type.msg
    =  Insert of 'k*'v*(unit -> (unit,'t)m)
    | Delete of 'k*(unit -> (unit,'t)m)
    | Find of 'k * ('v option -> (unit,'t)m)
    | Evictees of ('k * 'v entry) list

end


(*
module Pmap = struct
  (** A persistent map is formed from a B-tree and a detachable map
      (pcache).

      The "root" is a pair of the root for the persistent map and the root
      for the B-tree.

      When a pmap is constructed, we expect that the implementation requires
      some way to handle the pcache detach event followed by the B-tree
      rollup and the creation of the new root pair.

  *)

  type ('k,'v,'t) pmap = {
    find: 'k -> ('v option,'t) m;
    insert: 'k -> 'v -> (unit,'t) m;
    delete: 'k -> (unit,'t)m;
  }
end
*)

module Syncable_map = struct

  (** {2 Simple syncable map} *)

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
    batch: persist_mode -> [ `Delete of 'k | `Insert of ('k*'v) ] list -> (unit,'t)m;
    sync: unit -> ('ptr,'t)m;
    ksync: 'k -> ('ptr,'t)m;
    ks_sync: 'k list -> ('ptr,'t)m
  }


  (** {2 Extended syncable map} *)

  (* open Mt_intf *)
  include Mt_intf.Persist_mode
  
  (** Extended version; includes a "persist mode" (ie "now" or
     "later") and the sync operations *)
  type ('k,'v,'ptr,'t) syncable_map_with_pmode = {
    find: 'k -> ('v option,'t)m;
    insert: persist_mode -> 'k -> 'v -> (unit,'t)m;
    delete: persist_mode -> 'k -> (unit,'t)m;
    batch: persist_mode -> [ `Delete of 'k | `Insert of ('k*'v) ] list -> (unit,'t)m;
    sync: unit -> ('ptr,'t)m;
    ksync: 'k -> ('ptr,'t)m;
    ks_sync: 'k list -> ('ptr,'t)m
  }


  
end


module Msg_btree_rootman = struct

  (** A pair of roots, one for the detachable map, and one for the B-tree *)
  type 'blk_id msg_btree_rootman = {
    pcache_root: 'blk_id;
    btree_root: 'blk_id
  }

end


module Root_man_ops = struct

  type ('a,'t) root_man = {
    read_roots: unit -> ('a,'t)m;
    write_roots: ?sync:bool -> 'a -> (unit,'t)m;
  }

end
