(** This implements the cached persistent map API via an LRU on top of
   an uncached persistent map. *)

(**

Steps involved:

- Assume an uncached persistent map.

- Construct LRU. 

- Construct bi-directional queue between LRU and uncached pmap.

- Join components together

- Start the pcache thread so that it services the queue.

*)

open Tjr_monad
open Tjr_btree
open Tjr_btree.Block.BlkN
open Uncached_persistent_map_api

module Cache = Tjr_lru_cache.

let make_persistent_map
    ~monad_ops
    ~(uncached_pmap_ops:('k,'v,'t) uncached_pmap_ops)
  =


  (* construct LRU ----------------------------------- *)

  (* FIXME add Cache.Types module which can be opened without worrying
     about clobbering existing defns *)

  let cache_ops : ('k,'v,'t) Cache.cache_ops = failwith "FIXME" in

  let cached_map_ops = (
    Cache.make_cached_map
      ~monad_ops
      ~map_ops:uncached_pmap_ops
      ~cache_ops
    @@ fun ~cached_map_ops ~evict_hook -> 
    (* NOTE that evict_hook is for testing; we can ignore here *)
    cached_map_ops)   
  in

  




(* [@@ocaml.warning "-27"] *)



(* FIXME for concurrency we need to be very clear that mrefs get
     updated atomically *)



(* FIXME get concurrency correct: LRU is thread safe, but Ukv is not.
*)

(* API -------------------------------------------------------------- *)

(* At this point, we have a cached_map_ops (insert_many is not
   efficient FIXME). But we still need to expose the sync operations.

   The operations are: sync a key/value; sync the entire kv map. This
   needs to be done in the Cache module. Probably worth splitting this
   out from tjr_btree.

*)


