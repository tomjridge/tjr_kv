(** Main types *)

module Btree_ops = struct
  (* FIXME also include "batch" op *)

  (** The operations supported by the B-tree. The B-tree should be
     uncached. *)
  (* $(PIPE2SH("""sed -n '/type[ ].*btree_ops = /,/}/p' >GEN.btree_ops.ml_""")) *)
  type ('k,'v,'blk_id,'t) btree_ops = {
    find     : 'k -> ('v option,'t)m;
    insert   : 'k -> 'v -> (unit,'t)m;
    delete   : 'k -> (unit,'t)m;
    get_root : unit -> ('blk_id,'t)m;  
  }
  type ('k,'v,'blk_id,'t) t = ('k,'v,'blk_id,'t) btree_ops
end


module Msg_pc_bt = struct
  (** The type of messages sent from the pcache to the B-tree.

      This is a callback-oriented interface, with operations [find] and
      [detach] (to handle a list of operations... assumed to come from a
      detach operation via a map ie no duplicate keys)

  *)
  open Kvop
  (* open Blk_id_as_int *)

  (* $(PIPE2SH("""sed -n '/type[ ].*pc_bt_msg = /,/}/p' >GEN.pc_bt_msg.ml_""")) *)
  type ('k,'v,'blk_id,'t) pc_bt_msg = 
    | Find of 'k * ('v option -> (unit,'t) m)
    | Detach of {
        ops: ('k,'v) kvop list;
        new_pcache_hd_tl: 'blk_id (* the pointer to the pcache of length 1 *)
      }
end

module Msg_lru_pc = struct
  open Lru_msg (* otherwise error about kinds, which is really about
                  the constructors not being recognized *)

  (* $(PIPE2SH("""sed -n '/type[ ].*lru_pc_msg = /,/Evictees/p' >GEN.lru_pc_msg.ml_""")) *)
  type ('k,'v,'t) lru_pc_msg = ('k,'v,'t) lru_msg
    =  
      | Insert of 'k*'v*(unit -> (unit,'t)m)
      | Delete of 'k*(unit -> (unit,'t)m)
      | Find of 'k * ('v option -> (unit,'t)m)
      | Evictees of ('k * 'v Lru_entry.entry) list
            
    (** Debug for int,int *)
    let msg2string = 
      function
      | Insert(k,v,_) -> Printf.sprintf "Insert(%d,%d)" k v
      | Delete(k,_) -> Printf.sprintf "Delete(%d)" k
      | Find(k,_) -> Printf.sprintf "Find(%d)" k
      | Evictees es -> Printf.sprintf "Evictees(len=%d)" (List.length es)

end


(** {2 Messages} *)

open Blk_id_as_int

type ('k,'v,'t) pc_bt_msg = ('k,'v,blk_id,'t) Msg_pc_bt.pc_bt_msg

class type ['k,'v,'t] q_pc_bt = object
  method enqueue: ('k,'v,'t) pc_bt_msg -> (unit,'t)m
  method dequeue: unit -> (('k,'v,'t) pc_bt_msg,'t)m
  method len: unit -> int
end

type ('k,'v,'t) lru_pc_msg = ('k,'v,'t) Msg_lru_pc.lru_pc_msg

class type ['k,'v,'t] q_lru_pc = object
  method enqueue: ('k,'v,'t) lru_pc_msg -> (unit,'t)m
  method dequeue: unit -> (('k,'v,'t) lru_pc_msg,'t)m
  method len: unit -> int
end





(** {2 Factory} *)

(* FIXME for general use -- where there are many kv stores -- we want
   to incorporate a usedlist, and store the usedlist origin with the
   other origins *)


(* $(PIPE2SH("""sed -n '/type[ ].*kv_store[ ]/,/^>/p' >GEN.kv_store.ml_""")) *)
type ('k,'v,'blk_id,'t) kv_store = <
  btree_thread  : < start_btree_thread : unit -> (unit, 't)m >;
  lru_ops       : ('k, 'v, 't) mt_ops;
  pcache_thread : < start_pcache_thread : unit -> (unit, 't)m >;
  q_lru_pc      : ('k, 'v, 't) q_lru_pc;
  q_pc_bt       : ('k, 'v, 't) q_pc_bt;
  origin        : 'blk_id;
>
(** NOTE the two threads have to be started before various operations
   can complete; the lru_ops are the operations exposed to the user *)
  (* root_man      : (rt_blk, t) root_man;  *)
  (* rt_blk        : rt_blk  *)
  (* min_free      : min_free; *)
  (* blk_alloc     : (r, t) blk_allocator_ops; *)


type ('k,'v,'blk_id,'blk,'t,'params) kv_factory = <
  (* pcache_factory : ('k,'v,'blk_id,'blk,'kvop_map,'t) pcache_factory; *)

  with_:
    blk_dev_ops  : ('blk_id,'blk,'t)blk_dev_ops ->
    barrier      : (unit -> (unit,'t)m) ->
    sync         : (unit -> (unit,'t)m) -> 
    freelist_ops : ('blk_id,'t)freelist_ops_af ->
    params       : 'params -> 
    <
      create: unit -> ( ('k,'v,'blk_id,'t)kv_store,'t)m;
      (** Create an empty kv store, initializing blks etc *)
      
      restore: blk_id -> ( ('k,'v,'blk_id,'t)kv_store,'t)m;
      (** Restore from disk *)
    >
>



