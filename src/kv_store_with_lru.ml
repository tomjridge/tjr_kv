(** A KV store with an LRU cache frontend. *)


(** {2 Architecture}

{%html: 

<img src="https://docs.google.com/drawings/d/e/2PACX-1vTIXhyNa7dovQYXuJXBMwPQZU99-x_tRdTIH3SkMUDyPwbL31zExWXauT2hO-eRIUcnGP3RVHiSHrjt/pub?w=557&amp;h=428">

%}


We construct the following...

{3 LRU}

  - q_lru_pc, a msg queue from lru to pcache
  - Lru and lru_state
  - lru_thrd, which takes messages from lru.to_lower to enqueue on
    q_lru_pc; the lru interface itself can be used by many threads safely




{3 Pcache }

  - q_pc_bt, a msg queue from pcache to B-tree
  - Pcache and pcache_state
  - pc_thrd, which takes msgs from q_pc_btree and executes against pcache; also performs detach occasionally and enqueue messages to q_pc_bt


{3 B-tree}

  - B-tree
  - bt_thrd listening to q_pc_btree; also includes root manager functionality

*)

(* old doc?

{3 Functional store thread}

  - this maintains the "global" state, including locks etc
  - the queues are kept separate since they are implemented using
    mutation anyway FIXME perhaps prefer a "functional" version using
    the functional store
  - includes: lru_state; pcache_state; btree_state (a root pointer?)
  - also includes an Lwt_mvar for communication and implementation of
    with_state (?FIXME still true?)
*)

open Tjr_monad.With_lwt
open Lwt_aux  (* provides various msg queues *)
open Std_types
(* open Kv_intf  *)
open Kv_intf_v2
(* open Kv_config_runtime *)
let runtime_config = Kv_config_runtime.config
(* open Kv_profilers *)


class ['a] mutable_ = object
  val mutable the_ref = (Obj.magic () : 'a)
  val mutable is_set : bool = false
  method set x = the_ref <- x
  method get = 
    assert(is_set);
    the_ref
  method is_set = is_set
end

class ['a] set_once = object (self:'self)
  inherit ['a] mutable_
  method! set x = 
    assert(not(is_set));
    the_ref <- x
end

module type S = sig
  type k[@@deriving bin_io]
  type v[@@deriving bin_io]
  val k_cmp: k -> k -> int
  type r = Std_types.r[@@deriving bin_io]

  val k_size: int
  val v_size: int
  val r_size: int
end

module Make(S:S) = struct
  open S

  let marshalling_config : (k,v,r) Pcache_intf.marshalling_config = 
    (module S)

  module Btree_ = Tjr_btree_examples.Make_1.Make(struct
      include Std_types 
      include S 
      let cs = Tjr_btree_examples.Make_1.make_constants ~k_size ~v_size 
    end)

  module Pcache_ = Tjr_pcache.Make.Make(struct 
      include Std_types 
      include S 
      let marshalling_config = marshalling_config 
    end)

  module Lru_ = Tjr_lru_cache.Make.Make(struct
      type k = S.k
      let compare = S.k_cmp
      type v = S.v
      type t = lwt
      let monad_ops = monad_ops
      let async = async
      let event_ops = event_ops
    end)
  type mt_state = Lru_.mt_state

  type rt_blk = {
    mutable bt_rt: blk_id;
    mutable pc_hd: blk_id;
    mutable pc_tl: blk_id
  }

  type min_free = {
    mutable min_free_blk_id: blk_id;
  }

  let make ~blk_dev_ops ~(init0:[`From_disk | `Empty]) = 
    let open (struct

      let b0 = B.of_int 0 (** where we store the rt_blk *)

      let b1 = B.of_int 1 (** where the free list is stored *)

      let b2 = B.of_int 2 (** where the btree empty leaf is initially stored *)

      let b3 = B.of_int 3 (** pc_hd, pc_tl initially *)

      let b4 = B.of_int 4 (** first free block *)


      (** {2 Message queues} *)

      let q_lru_pc : (k,v) q_lru_pc = Tjr_mem_queue.With_lwt.make_as_object ()  
      let q_pc_bt : (k,v) q_pc_bt = Tjr_mem_queue.With_lwt.make_as_object ()


      (** {2 Root manager} *)

      let root_man : rt_blk Kv_intf_v2.root_man = 
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
          
      let min_free : min_free Kv_intf_v2.root_man = 
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

      let btree_thread = Btree_thread.(
          let x = Btree_.make ~blk_dev_ops ~blk_alloc ~root_ops:with_bt_rt in
          let module S = (val x) in  (* FIXME this is supposed to be uncached *)
          make_btree_thread ~q_pc_bt ~map_ops:S.map_ops_with_ls)

      let init = 
        match init0 with
        | `From_disk -> return ()
        | `Empty ->
          assert(rt_blk.bt_rt=b2);
          blk_dev_ops.write ~blk_id:b2 ~blk:(Btree_.empty_leaf_as_blk ())
    end)
    in
    init >>= fun () ->
    let open (struct

      (** {2 Pcache and pcache_thread } *)

      let init =
        match init0 with
        | `From_disk -> 
          Pcache_.read_initial_pcache_state 
            ~read_blk_as_buf:(fun r -> blk_dev_ops.read ~blk_id:r)
            ~root_ptr:rt_blk.pc_tl
            ~current_ptr:rt_blk.pc_hd 
        | `Empty -> 
          assert(rt_blk.pc_hd = b3 && rt_blk.pc_tl = b3);
          let pc = Pcache_state.empty_pcache_state 
              ~root_ptr:rt_blk.pc_tl
              ~current_ptr:rt_blk.pc_hd
              ~empty:Pcache_.kvop_map_ops.empty
          in
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

      let pcache_thread = Pcache_.(
          let pcache_ops = 
            make_pcache_ops_with_blk_dev 
              ~blk_dev_ops
              ~blk_alloc:blk_alloc.blk_alloc
              ~with_pcache 
          in
          Pcache_thread.make_pcache_thread 
            ~kvop_map_ops
            ~pcache_blocks_limit:config.pcache_blocks_limit
            ~pcache_ops
            ~q_lru_pc
            ~q_pc_bt)
      

      (** {2 LRU cache} *)

      let lru : (k,v,_) Lru.lru = 
        let open Lru in
        let args : (_,_,_) args = 
          object 
            method make_multithreaded_lru=Lru_.make_multithreaded_lru
            method q_lru_pc ()=q_lru_pc
          end
        in
        make_lru args

      let _ : unit = lru#set_initial_state 
          (Lru_.init_state ~max_size:config.lru_max_size ~evict_count:config.lru_evict_count)

      let lru_ops = lru#get_lru_ops ()
    end)
    in
    return (object
      method q_lru_pc      = q_lru_pc
      method q_pc_bt       = q_pc_bt
      method rt_blk        = rt_blk
      method min_free      = mf
      method root_man      = root_man
      method blk_alloc     = blk_alloc
      method btree_thread  = btree_thread
      method pcache_thread = pcache_thread
      method lru_ops       = lru_ops
    end)

end


module Int_int_ex = struct
  (**/**)
  module Internal = struct
    open Bin_prot.Std
    type k = int[@@deriving bin_io]
    type v = int[@@deriving bin_io]
    let k_cmp = Int_.compare
    type r = Std_types.r[@@deriving bin_io]
    let k_size = 9
    let v_size = 9
    let r_size = 9
  end
  module Internal2 = Make(Internal)
  (**/**)
  let make = Internal2.make

end

