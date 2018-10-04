(*

(** A key value store.

This is a key value store, using the following technologies:

- tjr_btree, a B-tree library
- tjr_pcache, a persistent cache
- an LRU write-back cache

 *)

(** 

Fix a type of keys and values (int, say).

Fix a block device.

Fix marshalling strategy.

Create a store.

Construct a B-tree.

Construct a persistent cache.

Link pcache to B-tree.

Construct an LRU write-back cache.

Link LRU to pcache.

*)

open Tjr_monad
open Tjr_btree
open Tjr_btree.Block.BlkN

type kk = int
type vv = int


(* fix monad_ops ---------------------------------------------------- *)

type phant  (* FIXME *)

type phant_passing = phant Tjr_monad.state_passing

let monad_ops : phant_passing Monad.monad_ops = failwith "FIXME"
(* FIXME lift monad_ops to Tjr_monad *)



(* block dev -------------------------------------------------------- *)

(* use a simple block dev on top of a file *)

(* FIXME we need to ensure that this file has no caching etc so that
   the B-tree and pcache can be certain of the behaviour *)

let filename = "./btree.blk"

let blk_sz = 4096

let fd_ops = failwith "FIXME need to fix monad to get mref for fd"
(* FIXME can't the fd just be a parameter rather than an mref? *)

let disk_ops : 't Tjr_btree.Disk_ops.block_device = 
  Tjr_btree.Disk_on_fd.make_disk ~monad_ops ~blk_sz ~fd_ops 



(* marshalling ------------------------------------------------------ *)

(* use binprot for the time being *)

let ps = Tjr_btree.Map_int_int.ps' ~blk_sz


(* store ------------------------------------------------------------ *)

(* we reserve block 0 for the roots etc; so blocks are numbered from 1 *)

let free_ops : (Block.blk_id,'t) Base_types.mref = failwith "FIXME"

let store_ops = 
  Tjr_btree.Disk_to_store.disk_to_store ~monad_ops ~ps ~disk_ops ~free_ops



(* B-tree ------------------------------------------------------------ *)

(* we need to do something with the new root when it is returned;
   presumably we can place in some ref *)
let page_ref_ops : ('r,phant_passing) Base_types.mref = failwith "FIXME"


let map_ops = 
  Store_to_map.store_ops_to_map_ops 
    ~monad_ops 
    ~constants:(ps#constants)
    ~cmp:(ps#cmp)
    ~page_ref_ops
    ~store_ops

let _ = map_ops  
(* FIXME key should be kk; we need to fix the int marshalling *)


(* pcache ----------------------------------------------------------- *)

type pcache_map

module Pcache_impl = struct

  open Tjr_pcache.Persistent_log

  (* in-memory kv map ops *)
  let map_ops : (kk,vv,pcache_map) kvop_map_ops  = failwith "FIXME"



  let insert = failwith "FIXME"  
  (* need to build up the pcache; FIXME add a method in persistent_log
     to avoid constructing all the lower layers *)


  let plog_state_ref = failwith "FIXME"
  (* transient plog state; need this in the state somewhere *)

  let plog_ops () =
    Tjr_pcache.Persistent_log.make_plog_ops
      ~monad_ops
      ~map_ops
      ~insert
      ~plog_state_ref 

end

let plog_ops : (kk, vv, pcache_map, 'a, phant_passing) Tjr_pcache.Persistent_log.plog_ops =
  Pcache_impl.plog_ops ()



(* link pcache to B-tree -------------------------------------------- *)

(* The idea is to periodically flush the tail of the pcache to the
   B-tree. This is essentially a generalization of the ImpFS GOM. *)

module Requires = struct
  module Bt_blk_id = struct type t = blk_id let int2t i = i let t2int t = t end
  module Pc_blk_id = Bt_blk_id
end


module Ukv' = Uncached_pcache_with_btree.Make(Requires)

let ukv_mref_ops : ('a,'t) Mref_plus.mref = failwith "FIXME"

let pcache_blocks_limit = 10  (* eg *)

let ukv_ops = 
  Ukv'.make_ukv_ops
    ~monad_ops
    ~btree_ops:map_ops
    ~pcache_ops:plog_ops 
    ~pcache_blocks_limit
    ~ukv_mref_ops
    ~kvop_map_bindings:(failwith "FIXME")
    ~bt_sync:(failwith "FIXME")
    ~sync_ukv_roots:(failwith "FIXME")

let _ = ukv_ops  (* FIXME key seems to be int here rather than kk *)
                

(* construct LRU and link to Ukv ----------------------------------- *)

open Tjr_lru_cache.Cache

(* FIXME for concurrency we need to be very clear that mrefs get
   updated atomically *)
let cache_ops : ('k,'v,'t) cache_ops = failwith "FIXME"

let cached_map_ops = 
  Tjr_lru_cache.Cache.make_cached_map
    ~monad_ops
    ~map_ops
    ~cache_ops
  @@ fun ~cached_map_ops ~evict_hook -> 
  (* NOTE that evict_hook is for testing; we can ignore here *)
  cached_map_ops
  [@@ocaml.warning "-27"]


(* FIXME get concurrency correct: LRU is thread safe, but Ukv is not.
   *)

(* API -------------------------------------------------------------- *)

(* At this point, we have a cached_map_ops (insert_many is not
   efficient FIXME). But we still need to expose the sync operations.

   The operations are: sync a key/value; sync the entire kv map. This
   needs to be done in the Cache module. Probably worth splitting this
   out from tjr_btree.

*)


*)
