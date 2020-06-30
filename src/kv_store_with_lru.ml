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


(** {2 Code} *)

(* open Tjr_monad.With_lwt *)
(* open Lwt_aux  (\* provides various msg queues *\) *)
open Shared_ctxt
open Kv_intf
(* open Kv_intf_v2 *)
(* open Kv_config_runtime *)
let runtime_config = Kv_config_runtime.config
(* open Kv_profilers *)



type rt_blk = {
  mutable bt_rt: blk_id;
  mutable pc_hd: blk_id;
  mutable pc_tl: blk_id
}

type min_free = { 
  mutable min_free_blk_id: blk_id;
}


(* $(PIPE2SH("""sed -n '/type[ ].*kv_store[ ]/,/^>/p' >GEN.kv_store.ml_""")) *)
type ('k,'v) kv_store = <
  blk_alloc     : (r, t) blk_allocator_ops;
  btree_thread  : < start_btree_thread : unit -> (unit, t)m >;
  lru_ops       : ('k, 'v, t) mt_ops;
  min_free      : min_free;
  pcache_thread : < start_pcache_thread : unit -> (unit, t)m >;
  q_lru_pc      : ('k, 'v, t) q_lru_pc;
  q_pc_bt       : ('k, 'v, t) q_pc_bt;
  root_man      : (rt_blk, t) root_man; 
  rt_blk        : rt_blk 
>
(**
{[
type ('k,'v) kv_store = <
  blk_alloc     : (r, t) blk_allocator_ops;
  btree_thread  : < start_btree_thread : unit -> (unit, t)m >;
  lru_ops       : ('k, 'v, t) mt_ops;
  min_free      : min_free;
  pcache_thread : < start_pcache_thread : unit -> (unit, t)m >;
  q_lru_pc      : ('k, 'v) q_lru_pc;
  q_pc_bt       : ('k, 'v) q_pc_bt;
  root_man      : rt_blk root_man; 
  rt_blk        : rt_blk 
>
]}

*)

(* FIXME remove deriving; prefer S' *)
module type S = sig
  type k[@@deriving bin_io]
  type v[@@deriving bin_io]
  val k_cmp: k -> k -> int
  type r = Shared_ctxt.r[@@deriving bin_io]

  val k_size: int
  val v_size: int
  val r_size: int
end

(* $(PIPE2SH("""sed -n '/module[ ]type[ ]S/,/^end/p' >GEN.S.ml_""")) *)
module type S' = sig
  (* NOTE specialized to shared_ctxt *)
  type k
  type v
  val k_cmp: k -> k -> int
  val k_mshlr: k bp_mshlr
  val v_mshlr: v bp_mshlr
end
  (* type r = Shared_ctxt.r *)
  (* val r_mshlr: r bp_mshlr (\* should be in Shared_ctxt *\) *)


module Make(S:S') = struct
  open S

(*  let marshalling_config : (k,v,r) Pcache_intf.marshalling_config = 
    (module S)
*)

  module S1 = struct
      include Shared_ctxt 
      include S 
      let r_mshlr = bp_mshlrs#r_mshlr
      let k_size = let open (val k_mshlr) in max_sz
      let v_size = let open (val v_mshlr) in max_sz
      let cs = Tjr_btree_examples.Make_1.make_constants ~k_size ~v_size 
    end
  module Btree_ = Tjr_btree.Pvt.Make_5.Make(S1)
  let btree_factory = Btree_.btree_factory

  module Pcache_ = Tjr_pcache.Make.Make(S1)
  let pcache_factory = Pcache_.pcache_factory

  module Lru_ = Tjr_lru_cache.Make.Make(S1)

  (* type mt_state = Lru_ .mt_state ; now just Lru_.lru  *)

  let make ~blk_dev_ops ~(init0:[`From_disk | `Empty]) = 
    let open (struct

      let b0 = B.of_int 0 (** where we store the rt_blk *)

      let b1 = B.of_int 1 (** where the free list is stored *)

      let b2 = B.of_int 2 (** where the btree empty leaf is initially stored *)

      let b3 = B.of_int 3 (** pc_hd, pc_tl initially *)

      let b4 = B.of_int 4 (** first free block *)


      (** {2 Message queues} *)

      let q_lru_pc : (k,v,_) q_lru_pc = Tjr_mem_queue.With_lwt.make_as_object ()  
      let q_pc_bt : (k,v,_) q_pc_bt = Tjr_mem_queue.With_lwt.make_as_object ()


      (** {2 Root manager} *)

      let root_man : (rt_blk,_) root_man = 
        Root_manager.make_3 ~blk_dev_ops ~blk_id:b0

      let init = 
        match init0 with
        | `From_disk -> 
          root_man#read_roots () >>= fun rt_blk ->
          return rt_blk
        | `Empty ->
          let rt_blk = { bt_rt=b2; pc_hd=b3; pc_tl=b3 } in
          root_man#write_roots rt_blk >>= fun () ->
          return rt_blk
    end)
    in
    init >>= fun rt_blk ->
    let open (struct
      
      (** {2 Blk allocator} *)
          
      let min_free : (min_free,_) root_man = 
        Root_manager.make_3 ~blk_dev_ops ~blk_id:b1

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

      let with_bt_rt = 
        let with_state f = 
          f ~state:rt_blk.bt_rt
            ~set_state:(fun bt_rt -> 
                rt_blk.bt_rt <- bt_rt; 
                return ())
        in
        { with_state }

      let fctry = btree_factory ~blk_dev_ops ~blk_allocator_ops:blk_alloc ~blk_sz:Shared_ctxt.blk_sz

      let btree_thread = Btree_thread.(
          let x = fctry#make_uncached with_bt_rt in          
          make_btree_thread ~q_pc_bt ~map_ops:x#map_ops_with_ls)

      let init = 
        match init0 with
        | `From_disk -> return ()
        | `Empty ->
          assert(rt_blk.bt_rt=b2);
          blk_dev_ops.write ~blk_id:b2 ~blk:(fctry#empty_leaf_as_blk)
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

