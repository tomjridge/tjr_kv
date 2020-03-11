(** A KV store with an LRU cache frontend. *)


(** {2 Architecture}

{%html: 

<a href="https://imgur.com/yQhDTnG"><img src="https://i.imgur.com/yQhDTnG.png" title="source: imgur.com" width='100%' /></a>


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
open Kv_intf 
open Intf_v2
open Kv_conf_runtime
(* open Kv_profilers *)


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

  (** {2 Message queues} *)
                  
  let q_lru_pc : (k,v) q_lru_pc = Tjr_mem_queue.With_lwt.make_as_object ()  
  let q_pc_bt : (k,v) q_pc_bt = Tjr_mem_queue.With_lwt.make_as_object ()


  (** {2 Blk_dev_ops ref} *)

  let blk_dev_ops_ref: (blk_id,blk,t) blk_dev_ops option ref = ref None
  let blk_dev_ops = lazy(
    assert(Option.is_some !blk_dev_ops_ref);
    !blk_dev_ops_ref |> Option.get)


  (** {2 Root manager} *)

  let roots : rt_blk option ref = ref None 
  let root_man : (rt_blk,t) Root_man_ops.root_man Lazy.t = lazy (
    assert(Option.is_some !roots);
    Root_manager.make_root_man ~monad_ops ~blk_ops ~blk_dev_ops:(Lazy.force blk_dev_ops))


  (** {2 Blk allocator} *)

  let blk_alloc : (r,t) blk_allocator_ops Lazy.t = lazy (
    let blk_alloc () = 
      assert(Option.is_some !roots);
      !roots |> Option.get |> fun x ->       
      roots:=Some({x with min_free=B.inc x.min_free});
      return x.min_free
    in
    let blk_free blk_id = return () in
    {blk_alloc;blk_free})



  (** {2 B-tree/btree ops/bt thread} *)

  module Btree_ = Tjr_btree_examples.Make_1.Make(struct
      include Std_types 
      include S 
      let cs = Tjr_btree_examples.Make_1.make_constants ~k_size ~v_size 
    end)

  let with_bt_rt = 
    let with_state f = 
      assert(Option.is_some !roots);
      f ~state:( (!roots |> Option.get).bt_rt )
        ~set_state:(fun bt_rt -> 
            !roots |> Option.get |> fun x -> 
            {x with bt_rt} |> fun x -> 
            roots:=Some x; return ())
    in
    { with_state }
      
  let btree_thread = lazy Btree_thread.(
      let blk_dev_ops = Lazy.force blk_dev_ops in
      let blk_alloc = Lazy.force blk_alloc in
      let x = Btree_.make ~blk_dev_ops ~blk_alloc ~root_ops:with_bt_rt in
      let module S = (val x) in  (* FIXME this is supposed to be uncached *)
      make_btree_thread ~q_pc_bt ~map_ops:S.map_ops_with_ls)


  (** {2 Pcache and pcache_thread } *)

  let marshalling_config : (k,v,r) Pcache_intf.marshalling_config = 
    (module S)

  module Pcache_ = Tjr_pcache.Make.Make(struct 
      include Std_types 
      include S 
      let marshalling_config = marshalling_config 
    end)

  (** NOTE don't set this directly; use set_pcache_state *)
  let pcache_state : Pcache_.pcache_state option ref = ref None 

  let set_pcache_state = fun s -> 
    pcache_state := Some s; 
    assert(Option.is_some !roots);
    (* also update roots *)
    !roots|>Option.get|>fun x ->
    { x with pc_hd=s.root_ptr; pc_tl=s.current_ptr } |> fun x ->
    roots:=Some x;
    ()

  let with_pcache = 
    let with_state f = 
      assert(Option.is_some !pcache_state);
      f ~state:( !pcache_state |> Option.get )
        ~set_state:(fun s ->
            set_pcache_state s;
            return ())
    in
    { with_state }

  let pcache_thread = lazy Pcache_.(
      let pcache_ops = 
        make_pcache_ops_with_blk_dev 
          ~blk_dev_ops:(Lazy.force blk_dev_ops) 
          ~blk_alloc:(Lazy.force blk_alloc).blk_alloc 
          ~with_pcache 
      in
      Pcache_thread.make_pcache_thread 
        ~kvop_map_ops
        ~pcache_blocks_limit:config.pcache_blocks_limit
        ~pcache_ops
        ~q_lru_pc
        ~q_pc_bt)


  (** {2 LRU cache} *)

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
  include Internal2

end
