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
type ('k,'v) kv_store = <
  btree_thread  : < start_btree_thread : unit -> (unit, t)m >;
  lru_ops       : ('k, 'v, t) mt_ops;
  pcache_thread : < start_pcache_thread : unit -> (unit, t)m >;
  q_lru_pc      : ('k, 'v, t) q_lru_pc;
  q_pc_bt       : ('k, 'v, t) q_pc_bt;
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

      create: unit -> ( ('k,'v)kv_store,'t)m;
      (** Create an empty kv store, initializing blks etc *)

      restore_from_disk: unit -> ( ('k,'v)kv_store,'t)m;
      (** Restore from disk, using blk 0 *)
    >
>


(** {2 Code} *)

let runtime_config = Kv_config_runtime.config

module type S = sig
  type k
  type v
  type buf = ba_buf
  type blk = ba_buf
  type blk_id = Shared_ctxt.r
  type kvop_map
  type t
  val monad_ops: t monad_ops

  val pcache_factory : (k, v, r, buf, kvop_map, t) pcache_factory

  type leaf
  type node 
  type dnode
  type ls
  type wbc
  val btree_factory : (k,v,r,t,leaf,node,dnode,ls,blk,wbc) btree_factory

  val freelist_ops : (blk_id,t) freelist_ops_af

  type lru
  val lru_factory : (k,v,lru,t) lru_factory

  val blk_dev_ops : (blk_id,blk,t) blk_dev_ops
  val barrier     : unit -> (unit,t)m

  val root_manager : (blk_id,blk,t) Root_manager.root_manager
end

module Make(S:S) = struct
  module S=S
  open S

  let ( >>= ) = monad_ops.bind

  let return = monad_ops.return

  (** {2 Message queues} *)

  let q_lru_pc : (k,v,_) q_lru_pc = Tjr_mem_queue.With_lwt.make_as_object ()  

  let q_pc_bt : (k,v,_) q_pc_bt = Tjr_mem_queue.With_lwt.make_as_object ()

  let root_man = root_manager#with_ ~blk_dev_ops    
      
(*
- (b_origin): pcache origin and btree root
- (b_pcache): empty pcache
- (b_empty_btree): empty btree
*)

  let blk_alloc = freelist_ops

  let create () = 

    (* btree first *)
    freelist_ops.blk_alloc () >>= fun b_empty_btree -> 
    btree_factory#write_empty_leaf ~blk_dev_ops ~blk_id:b_empty_btree >>= fun () ->
    btree_factory#uncached ~blk_dev_ops ~blk_alloc ~init_btree_root:b_empty_btree |> fun btree -> 
    
    (* then pcache *)
    freelist_ops.blk_alloc () >>= fun b_pcache -> 
    let simple_plist_factory = pcache_factory#simple_plist_factory in
    let plist_factory = simple_plist_factory#plist_factory in
    let pl_with = plist_factory#with_blk_dev_ops ~blk_dev_ops ~barrier in
    pl_with#create b_pcache >>= fun created_pl -> 
    created_pl#plist_ops.get_origin () >>= fun pl_origin -> 

    (* then the origin *)
    let open Root_manager in
    freelist_ops.blk_alloc () >>= fun b_origin -> 
    let origin = { pcache_root=pl_origin; btree_root=b_empty_btree } in
    root_man#write_origin ~blk_id:b_origin ~origin >>= fun () ->
    
    (* then the lru *)
    let lru : (k,v,_) Lru.lru = lru_factory#make
      let open Lru in
      let fctry = 
        Lru_.lru_factory 
                ~max_size:config.lru_max_size 
                ~evict_count:config.lru_evict_count
        in
        let args : (_,_,_) args = 
          object 
            method make_multithreaded_lru=fun ~with_lru ~to_lower -> 
              fctry#make ~with_lru ~to_lower
            method q_lru_pc ()=q_lru_pc
          end
        in
        make_lru args

      let lru_ops = lru#get_lru_ops ()
    let 

    (* then link 
    
  
  
end


(* $(PIPE2SH("""sed -n '/module[ ]type[ ]S/,/^end/p' >GEN.S.ml_""")) *)
module type S = sig
  type k
  type v

  val k_cmp   : k -> k -> int
  val k_mshlr : k bp_mshlr
  val v_mshlr : v bp_mshlr
end
(** NOTE specialized to shared_ctxt *)

(*
module Make(S:S) = struct
  open S

  (** First step is to invoke the relevant functors to construct the
     btree, pcache and lru *)

  module S1 = struct
    include Shared_ctxt 
    include S 
    let r_mshlr = bp_mshlrs#r_mshlr
    let k_size = let open (val k_mshlr) in max_sz
    let v_size = let open (val v_mshlr) in max_sz
    let cs = Tjr_btree.Bin_prot_marshalling.make_constants ~blk_sz ~k_size ~v_size
  end

  module Btree_ = Tjr_btree.Make_6.Make_v1(S1)
  let btree_factory = Btree_.btree_factory

  module Pcache_ = Tjr_pcache.Make.Make(S1)
  let pcache_factory = Pcache_.pcache_factory

  module Lru_ = Tjr_lru_cache.Make.Make(S1)


  (** {2 Message queues} *)

  let q_lru_pc : (k,v,_) q_lru_pc = Tjr_mem_queue.With_lwt.make_as_object ()  
  let q_pc_bt : (k,v,_) q_pc_bt = Tjr_mem_queue.With_lwt.make_as_object ()



  module With_(W:sig val blk_dev_ops:(blk_id,blk,t)blk_dev_ops end) = struct
    open W

    (** {2 Root manager} *)

    let root_man = Root_manager.root_managers#for_lwt_ba_buf#with_ ~blk_dev_ops    

    let create () = 
      root_man#write_origin ~blk_id:b_origin 
        ~origin:{pcache_root=b_empty_pcache;btree_root=b_empty_btree} >>= fun () -> 
      pcache_factory
      

    let restore_from_origin (o:Origin.t) = ()
      
      
  let make ~blk_dev_ops ~(init0:[`From_disk | `Empty]) = 
    let open (struct





      let init = 
        match init0 with
        | `From_disk -> root_man#read_origin b0
        | `Empty ->
          let origin = Root_manager.{ pcache_root=b3; btree_root=b2 } in
          root_man#write_origin ~blk_id:b0 ~origin >>= fun () ->
          return origin
    end)
    in
    init >>= fun rt_blk ->
    let open (struct
      
      (** {2 Blk allocator} *)
          
      let min_free : (min_free,_) root_man = 
        (* Root_manager.make_3 ~blk_dev_ops ~blk_id:b1 *)
        failwith "FIXME"

      let init = 
        match init0 with
        | `From_disk ->
          min_free#read_roots () >>= fun mf ->
          return mf
        | `Empty -> 
          let mf = { min_free_blk_id=b4 } in
          min_free#write_roots mf >>= fun () ->
          return mf
    end)
    in
    init >>= fun mf ->
    let open (struct

      let blk_alloc : (r,t) blk_allocator_ops = 
        let blk_alloc () = 
          let r = mf.min_free_blk_id in
          mf.min_free_blk_id <- B.inc mf.min_free_blk_id;
          return r
        in
        let blk_free blk_id = return () in
        {blk_alloc;blk_free}
        

      (** {2 B-tree/btree ops/bt thread} *)

(*
      let with_bt_rt = 
        let with_state f = 
          f ~state:rt_blk.bt_rt
            ~set_state:(fun bt_rt -> 
                rt_blk.bt_rt <- bt_rt; 
                return ())
        in
        { with_state }
*)

      let x = btree_factory#uncached ~blk_dev_ops ~blk_alloc ~init_btree_root:rt_blk.bt_rt

      let btree_thread = Btree_thread.(
          make_btree_thread ~q_pc_bt ~map_ops:x#map_ops_with_ls)

      let init = 
        match init0 with
        | `From_disk -> return ()
        | `Empty ->
          assert(rt_blk.bt_rt=b2);
          btree_factory#write_empty_leaf ~blk_dev_ops ~blk_id:b2
            
    end)
    in
    init >>= fun () ->
    let open (struct

      (** {2 Pcache and pcache_thread } *)

      let fctry = pcache_factory ~blk_alloc:blk_alloc.blk_alloc
      let fctry1 = fctry#with_blk_dev_ops ~blk_dev_ops

      let init =
        match init0 with
        | `From_disk -> fctry1#read_initial_pcache_state (rt_blk.pc_hd)
        | `Empty -> 
          assert(rt_blk.pc_hd = b3 && rt_blk.pc_tl = b3);
          let pc = fctry#empty_pcache_state ~ptr:b3  in
          return pc
    end)
    in
    init >>= fun pc->
    
    let open (struct
    
      let pcache_state = ref pc

      let set_pcache_state = fun s -> 
        pcache_state:= s; 
        rt_blk.pc_hd <- s.root_ptr;
        rt_blk.pc_tl <- s.current_ptr;
        ()

      let with_pcache = 
        let with_state f = 
          f ~state:!pcache_state
            ~set_state:(fun s ->
                set_pcache_state s;
                return ())
        in
        { with_state }

      let config = Lazy.force runtime_config

      let pcache_thread = 
        let pcache_ops = fctry1#make_pcache_ops#with_state with_pcache in
        Pcache_thread.make_pcache_thread
          ~kvop_map_ops:fctry#kvop_map_ops
          ~pcache_blocks_limit:config.pcache_blocks_limit
          ~pcache_ops
          ~q_lru_pc
          ~q_pc_bt


      (** {2 LRU cache} *)

      let lru : (k,v,_) Lru.lru = 
        let open Lru in
        let fctry = 
              Lru_.lru_factory 
                ~max_size:config.lru_max_size 
                ~evict_count:config.lru_evict_count
        in
        let args : (_,_,_) args = 
          object 
            method make_multithreaded_lru=fun ~with_lru ~to_lower -> 
              fctry#make ~with_lru ~to_lower
            method q_lru_pc ()=q_lru_pc
          end
        in
        make_lru args

      let lru_ops = lru#get_lru_ops ()
    end)
    in
    let obj : (_,_)kv_store = object
      method q_lru_pc      = q_lru_pc
      method q_pc_bt       = q_pc_bt
      method rt_blk        = rt_blk
      method min_free      = mf
      method root_man      = root_man
      method blk_alloc     = blk_alloc
      method btree_thread  = btree_thread
      method pcache_thread = pcache_thread
      method lru_ops       = lru_ops
    end
    in
    return obj
(**
{[
blk_dev_ops:(blk_id, Bigstring.t, t) blk_dev_ops ->
init0:[ `Empty | `From_disk ] ->
((k, v) kv_store, t) m
]}
*)

  let _ = make

end


module Int_int_ex = struct
  (**/**)
  module Internal = struct
    include Shared_ctxt
    type k = int
    type v = int
    let k_cmp = Int_.compare
    let k_mshlr = bp_mshlrs#int_mshlr
    let v_mshlr = bp_mshlrs#int_mshlr
  end
  module Internal2 = Make(Internal)
  (**/**)
  let make = Internal2.make
end

*)


(** {2 Initial disk layout}

- (b_origin): pcache root and btree root
- (b_empty_pcache): empty pcache
- (b_empty_btree): empty btree

*)

let b_origin = B.of_int 0 

let b_empty_pcache = B.of_int 1

let b_empty_btree = B.of_int 2 

let b_freelist_origin = B.of_int 3 

let b_first_free_blk = B.of_int 4 

