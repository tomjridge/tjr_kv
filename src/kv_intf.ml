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

  include Tjr_lru_cache.Im_intf.Entry.Export

  (* FIXME this should probably be moved to fs_shared *)
  type ('k,'v,'t) lru_dmap_msg
    = ('k,'v,'t) Tjr_lru_cache.Mt_intf.msg
    =  Insert of 'k*'v*(unit -> (unit,'t)m)
    | Delete of 'k*(unit -> (unit,'t)m)
    | Find of 'k * ('v option -> (unit,'t)m)
    | Evictees of ('k * 'v Tjr_lru_cache.Im_intf.entry) list

end


(*
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
    dmap_root: 'blk_id;
    btree_root: 'blk_id
  }

end

(** These types are common to almost all the components (well, perhaps
   the root manager deals with blks rather than kvrt) *)
module type KVRT = sig
  type k
  type v
  type r

  type t

  (** Once the [t] monad type is fixed, the mutex and cvar types are usually also fixed *)
  type mutex
  type cvar 

  (** The type of the monadic yield *)
  type yield_t
    
  (** The type of the monadic sleep *)
  type sleep_t
end  

(** The following module is for system architecture documentation purposes *)
module Ctxt(Pre:KVRT) = struct
  open Memq_intf
  include Pre

  module type LRU_DMAP = sig
    type msg1 = (k,v,t) Msg_lru_dmap.lru_dmap_msg
    type q1 = (mutex,cvar,msg1) queue
    val q_lru_dmap_ops : (msg1,q1,t) memq_ops
  end

  
  (* NOTE leaves in the ctxt tree can be mod types rather than mods *)
  (** The main types and values in scope at the Lru layer *)
  module type LRU = sig
    open Syncable_map
    type lru_ops = (k,v,r,t) syncable_map_with_pmode
    val lru_ops: lru_ops
    (* NOTE no lru thread; lru_callback_ops are used instead *) 
  end


  module With_blk_id(Blk_id:sig type blk_id = r end) = struct
    open Blk_id

    module type DMAP_BTREE = sig
      (* FIXME this msg type also depends on blk_id *)
      type msg2 = (k,v,t) Msg_dmap_bt.dmap_bt_msg
      type q2 = (mutex,cvar,msg2) queue
      val q_dmag_bt_ops : (msg2,q2,t) memq_ops
    end

    (** The main types and values in scope at the detachable map *)
    module type DMAP = sig   
      (** NOTE this thread "never" terminates; it takes msgs from the
          lru->dmap queue, and applies them to the log; occasionally the
          log is detached and the result is put on the dmap->btree queue
      *)
      val dmap_thread: yield:yield_t -> sleep:sleep_t -> unit -> ('a,t)m
    end

    module type BTREE_ROOTMAN = sig
      open Msg_btree_rootman
      type msg3 = blk_id msg_btree_rootman
    end

    module type BTREE = sig
      (** This thread takes items of dmap->btree, and executes against the btree *)
      val btree_thread: yield:yield_t -> sleep:sleep_t -> unit -> ('a,t)m        
    end

    module type ROOTMAN = sig
      val rootman_thread: yield:yield_t -> sleep:sleep_t -> unit -> ('a,t)m
    end
  end

end

(** Documentation of the various state types of the components *)
module States(Pre:KVRT) = struct
  include Pre

  module type LRU = sig
    type k_map
    type t_map
    type nonrec lru_state = (k,v,k_map,t_map,t) Mt_state_type.lru_state
  end

  module With_blk_id(Blk_id:sig type blk_id = r end) = struct
    open Blk_id

    module type DMAP = sig
      open Dmap_types
      open Dcl_types

      (** NOTE typically the internal state is the same for pl and pcl,
          and is a buffer and an int *)
      type pl_and_pcl_internal_state 

      type kvop_map

      (** dcl_state is start_block, current_block etc *)
      type nonrec dcl_state = (r,kvop_map) dcl_state
      type nonrec detach_info = (k,v,r) detach_info
      type nonrec dmap_ops = (k,v,r,t) dmap_ops

      type dmap_state = pl_and_pcl_internal_state * dcl_state
    end


    module type BTREE = sig
      (** These fixed types are placeholders *)
      type t1
      type t2

      (** NOTE: our examples use a file descriptor as a reference to the "block device" *)
      type fd

      (** This is the state held in memory while the B-tree executes *)
      type btree_state = {
        blk_allocator_state:t1;
        fstore_state:t2;
        btree_root_blk:blk_id;
        blk_dev:fd
      }
    end


    module type ROOTMAN = sig
      type fd
      (** This is the state held in memory and presumably mirrored
          somewhere on disk (at least, for the roots) eg in block 0 *)
      type rootman_state = {
        blk_dev:fd;
        dmap_root:blk_id;
        btree_root:blk_id;
      }        
    end

    (** NOTE The system state is composed of the above, plus the
       contents of the queues, plus the contents of the caches (eg the
       store cache) *)
  end
end
