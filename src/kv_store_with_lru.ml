(** A KV store with an LRU cache frontend. *)


(** {2 Architecture}

{%html:

<img src="https://docs.google.com/drawings/d/e/2PACX-1vTIXhyNa7dovQYXuJXBMwPQZU99-x_tRdTIH3SkMUDyPwbL31zExWXauT2hO-eRIUcnGP3RVHiSHrjt/pub?w=557&amp;h=428">

%}


We construct the following...


{3 Queues: q_lru_pc and q_pc_bt}

  - q_lru_pc, a msg queue from the lru to the pcache
  - q_pc_bt, a msg queue from the pcache to the B-tree


{3 Blk allocator}

  - provides blk alloc/free


{3 LRU}

  - lru_ops, concurrent-safe map operations


{3 Pcache }

  - pcache_thread, which takes msgs from q_pc_bt and executes against the pcache; also performs detach occasionally and enqueues messages to q_pc_bt


{3 B-tree}

  - btree_thread, listening to q_pc_bt and executing operations against the B-tree


{3 Root manager}

  - root_man, which is responsible for persisting the roots for the B-tree and the pcache

*)

open Shared_ctxt
open Kv_intf
(* open Root_manager *)

(** {2 Types} *)

(* $(PIPE2SH("""sed -n '/type[ ].*kv_store[ ]/,/^>/p' >GEN.kv_store.ml_""")) *)
type ('k,'v,'blk_id) kv_store = <
  btree_thread  : < start_btree_thread : unit -> (unit, t)m >;
  lru_ops       : ('k, 'v, t) mt_ops;
  pcache_thread : < start_pcache_thread : unit -> (unit, t)m >;
  q_lru_pc      : ('k, 'v, t) q_lru_pc;
  q_pc_bt       : ('k, 'v, t) q_pc_bt;
  origin        : 'blk_id;
>
(** NOTE the two threads have to be started before various operations
   can complete; the lru_ops are the operations exposed to the user *)
  (* root_man      : (rt_blk, t) root_man;  *)
  (* rt_blk        : rt_blk  *)
  (* min_free      : min_free; *)
  (* blk_alloc     : (r, t) blk_allocator_ops; *)




(** {2 Factory} *)

(* FIXME for general use -- where there are many kv stores -- we want
   to incorporate a usedlist, and store the usedlist origin with the
   other origins *)

type ('k,'v,'blk_id,'blk,'buf,'kvop_map,'t) kv_factory = <

  pcache_factory : ('k,'v,'blk_id,'buf,'kvop_map,'t) pcache_factory;

  (* FIXME want barrier and sync as args *)
  with_:
    blk_dev_ops:('blk_id,'blk,'t)blk_dev_ops ->
    freelist_ops : ('blk_id,'t)Shared_freelist.freelist_ops ->
    <
      create: unit -> ( ('k,'v,'blk_id)kv_store,'t)m;
      (** Create an empty kv store, initializing blks etc *)
      
      restore: blk_id -> ( ('k,'v,'blk_id)kv_store,'t)m;
      (** Restore from disk *)
    >
>


(** {2 Code} *)

let runtime_config = Kv_config_runtime.config

module type S = sig
  type t = lwt
  val monad_ops      : t monad_ops
  type mutex
  type cvar
  val mutex_ops      : (mutex,cvar,t) mutex_ops
  val async          : t async
  val yield          : unit -> (unit,t)m
  val event_ops      : t event_ops

  type k
  type v
  type buf = ba_buf
  type blk = ba_buf
  type blk_id = Shared_ctxt.r
  type r = Shared_ctxt.r

  type kvop_map
  val pcache_factory : (k, v, r, buf, kvop_map, t) pcache_factory

  type leaf
  type node
  type dnode
  type ls
  type wbc
  val btree_factory  : (k,v,r,t,leaf,node,dnode,ls,blk,wbc) btree_factory

  val freelist_ops   : (blk_id,t) freelist_ops_af

  type lru
  val lru_factory    : (k,v,lru,t) lru_factory

  val blk_dev_ops    : (blk_id,blk,t) blk_dev_ops
  val barrier        : unit -> (unit,t)m
  val sync           : unit -> (unit,t)m

  val root_manager   : (blk_id,blk,t) Root_manager.root_manager
end

module Make(S:S) = struct
  module S=S
  open S

  let kvop_map_ops = pcache_factory#kvop_map_ops

  let ( >>= ) = monad_ops.bind

  let return = monad_ops.return

  let root_man = root_manager#with_ ~blk_dev_ops

  let blk_alloc = freelist_ops

  module Btree_thread = Btree_thread.Make(S)

  module Pcache_thread = Pcache_thread.Make(S)

  let pc_with = pcache_factory#with_ ~blk_dev_ops ~barrier ~freelist_ops

(*
- (b_origin): pcache origin and btree root
- (b_pcache): empty pcache
- (b_empty_btree): empty btree
*)

  let with_ ~params = 
    let open (struct
      let lru_params = params#lru_params 
      let pcache_blocks_limit = params#pcache_blocks_limit
  
      let create () : (_ kv_store,_)m =

        (* queues *)
        let q_lru_pc : (k,v,_) q_lru_pc = Tjr_mem_queue.With_lwt.make_as_object () in
        let q_pc_bt : (k,v,_) q_pc_bt = Tjr_mem_queue.With_lwt.make_as_object () in

        (* btree first *)
        freelist_ops.blk_alloc () >>= fun b_empty_btree ->
        btree_factory#write_empty_leaf ~blk_dev_ops ~blk_id:b_empty_btree >>= fun () ->
        btree_factory#uncached
          ~blk_dev_ops ~blk_alloc ~init_btree_root:b_empty_btree |> fun btree_o ->
        
        (* then pcache *)
        pc_with#create () >>= fun pcache_ops ->
        pcache_ops.get_origin () >>= fun pcache_origin ->
        
        (* then the origin *)
        let open Root_manager in
        freelist_ops.blk_alloc () >>= fun b_origin ->
        let write_origin origin = root_man#write_origin ~blk_id:b_origin ~origin in
        let origin = { pcache_origin; btree_root=b_empty_btree } in
        write_origin origin >>= fun () ->

        (* then the lru *)
        let to_lower msg = q_lru_pc#enqueue msg in
        let lru_ref = ref @@
          lru_factory#empty
            ~max_size:lru_params#max_size
            ~evict_count:lru_params#evict_count
        in
        (* FIXME the lru has to be locked *)
        with_locked_ref ~monad_ops ~mutex_ops lru_ref >>= fun with_state ->
        let lru_ops = lru_factory#make_ops ~with_state ~to_lower in

        (* then the btree thread and the pcache thread *)
        let btree_thread = Btree_thread.make_btree_thread
            ~write_origin
            ~q_pc_bt
            ~map_ops_bt:btree_o#map_ops_with_ls
            ~sync_bt:(fun () ->
                (* NOTE we need to sync the blk_dev *)
                sync () >>= fun () ->
                btree_o#get_btree_root ())
        in

        let pcache_thread =
          Pcache_thread.make_pcache_thread
            ~kvop_map_ops
            ~pcache_blocks_limit
            ~pcache_ops
            ~q_lru_pc
            ~q_pc_bt
        in
        let _ = pcache_thread in

        let obj : _ kv_store = object
          method btree_thread=btree_thread
          method pcache_thread=pcache_thread
          method lru_ops=lru_ops
          method q_lru_pc=q_lru_pc
          method q_pc_bt=q_pc_bt
          method origin=b_origin
        end
        in
        return obj


      let restore b_origin : (_ kv_store,_)m = 
        (* queues *)
        let q_lru_pc : (k,v,_) q_lru_pc = Tjr_mem_queue.With_lwt.make_as_object () in
        let q_pc_bt : (k,v,_) q_pc_bt = Tjr_mem_queue.With_lwt.make_as_object () in

        (* origin first *)
        root_man#read_origin b_origin >>= fun origin -> 

        (* then the origin *)
        let open Root_manager in
        let write_origin origin = root_man#write_origin ~blk_id:b_origin ~origin in

        (* btree *)
        btree_factory#uncached
          ~blk_dev_ops ~blk_alloc ~init_btree_root:origin.btree_root |> fun btree_o ->

        (* then pcache *)
        pc_with#restore ~hd:origin.pcache_origin.hd >>= fun pcache_ops ->
        
        (* then lru *)
        let to_lower msg = q_lru_pc#enqueue msg in
        let lru_ref = ref @@
          lru_factory#empty
            ~max_size:lru_params#max_size
            ~evict_count:lru_params#evict_count
        in
        (* FIXME the lru has to be locked *)
        with_locked_ref ~monad_ops ~mutex_ops lru_ref >>= fun with_state ->
        let lru_ops = lru_factory#make_ops ~with_state ~to_lower in



        (* then the btree thread and the pcache thread *)
        let btree_thread = Btree_thread.make_btree_thread
            ~write_origin
            ~q_pc_bt
            ~map_ops_bt:btree_o#map_ops_with_ls
            ~sync_bt:(fun () ->
                (* NOTE we need to sync the blk_dev *)
                sync () >>= fun () ->
                btree_o#get_btree_root ())
        in

        let pcache_thread =
          Pcache_thread.make_pcache_thread
            ~kvop_map_ops
            ~pcache_blocks_limit
            ~pcache_ops
            ~q_lru_pc
            ~q_pc_bt
        in
        let _ = pcache_thread in

        let obj : _ kv_store = object
          method btree_thread=btree_thread
          method pcache_thread=pcache_thread
          method lru_ops=lru_ops
          method q_lru_pc=q_lru_pc
          method q_pc_bt=q_pc_bt
          method origin=b_origin
        end
        in
        return obj
       
    end)
    in
    object
      method create=create
      method restore=restore
    end

end
