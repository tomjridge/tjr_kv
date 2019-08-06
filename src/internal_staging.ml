(** INTERNAL PLEASE IGNORE This is another attempt to isolate the
   various stages of type construction. *)

open Kv_intf

module type M = sig
  type t
  val monad_ops: t monad_ops
end


module type B1 = sig
  type blk_id
  type blk
  val blk_ops: blk blk_ops
end

module type B2 = sig
  include M
  include B1
  (** NOTE typically blk_dev_ops requires eg a file_descr *)
  val blk_dev_ops: (blk_id,blk,t)blk_dev_ops
  val blk_layer: (blk,(blk_id,blk,t)blk_dev_ops)blk_layer
end


module type K1 = sig
  type k
  val compare_k: k -> k -> int
  type v
end

module type KB1 = sig
  include B1
  include K1
end

module type KB2 = sig
  include KB1
  (* B-tree marshal *)
end

module type KB3 = sig
  (* pcache marshal *)
end


(** argument to B-tree make *)
module type BT1 = sig
  include M
  include K1
  include B1 (* only need blk_id *)
  type r = blk_id
end

module I = Isa_btree.Isa_btree_intf
module B = Tjr_btree.Btree_intf

(** result of B-tree make; NOTE that B.disk_ops contains the dnode
   marshalling code, and that this is an argument to most of the
   constructor functions in Tjr_btree.Make *)
module type BT2 = sig
  include BT1
  type leaf
  type node
  type leaf_stream
  type dnode = (node,leaf)I.dnode
  val leaf_ops: (k,v,leaf)I.leaf_ops
  val node_ops: (k,r,node)I.node_ops
  type disk_ops = (r,t,dnode,blk) B.disk_ops
  type store_ops = (r,dnode,t) I.store_ops
  type pre_btree_ops = (k,v,r,t,leaf,node,leaf_stream) I.pre_btree_ops
  val make_something: disk_ops -> unit
end



(** {2 Older version} *)


(** These types are common to almost all the components (well, perhaps
   the root manager deals with blks rather than kvrt) *)
module type KVRT = sig
  type k
  type v
  type r
  type blk_id = r

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


  module type DMAP_BTREE = sig
    type msg2 = (k,v,blk_id,t) Msg_dmap_bt.dmap_bt_msg
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


(** Documentation of the various state types of the components *)
module States(Pre:KVRT) = struct
  include Pre

  module type LRU = sig
    type k_map
    type t_map
    type nonrec lru_state = (k,v,k_map,t_map,t) Mt_state_type.mt_state
  end

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




