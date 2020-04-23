(** Version using {!Kv_intf_v3} interfaces *)

(* NOTE this code is mostly copied from kv_store_with_lru *)
[@@@warning "-33"]

open Tjr_monad.With_lwt
open Lwt_aux  (* provides various msg queues *)
open Std_types
open Kv_intf 
open Kv_intf_v3


module type S = Kv_store_with_lru.S 

(*
module Make(S:S) = struct
  open S


  (** {2 Message queues} *)
                  
  let q_lru_pc : (k,v) Kv_intf_v2.q_lru_pc = Tjr_mem_queue.With_lwt.make_as_object ()  
  let q_pc_bt : (k,v) Kv_intf_v2.q_pc_bt = Tjr_mem_queue.With_lwt.make_as_object ()


  (** {2 Blk_dev_ops ref} *)

  let blk_dev_ops_ref: (blk_id,blk,t) blk_dev_ops option ref = ref None

  let set_blk_dev_ops blk_dev_ops = blk_dev_ops_ref := Some blk_dev_ops

  let get_blk_dev_ops () = 
    assert(Option.is_some !blk_dev_ops_ref);
    Option.get !blk_dev_ops_ref


  module Make_root_blk_ops(S:sig type rb[@@deriving bin_io] end) = struct
    include S

    let write_rb ~blk_id rb =
      let blk_dev_ops = get_blk_dev_ops () in
      (* FIXME have a single buf_create, specialized to std_type and buf_sz *)
      let buf = buf_create () in
      bin_write_rb buf ~pos:0 rb |> fun _ ->
      blk_dev_ops.write ~blk_id ~blk:buf

    let read_rb: blk_id:blk_id -> (rb,t)m = fun ~blk_id ->
      let blk_dev_ops = get_blk_dev_ops () in
      blk_dev_ops.read ~blk_id >>= fun blk ->
      bin_read_rb blk ~pos_ref:(ref 0) |> fun rb ->
      return rb
  end

  module X = Make_root_blk_ops(struct type rb = rt_blk[@@deriving bin_io] end)



  (** {2 The root block, where roots (apart from freelist) are stored} *)
  
  let r0 = ref None

  let set_rt blk_id = r0:=Some blk_id

  let get_rt () = 
    assert(Option.is_some !r0);
    Option.get !r0


  let rb_ref = ref None

  let set_rt_blk rb = rb_ref:=Some rb

  let get_rb () = 
    assert(Option.is_some !rb_ref);
    Option.get !rb_ref

  let sync_rt_blk_from_disk () =
    get_rt () |> fun blk_id -> 
    X.read_rb ~blk_id >>= fun rb ->
    rb_ref:=Some rb; return ()

  let sync_rt_blk_to_disk () =
    get_rt () |> fun blk_id -> 
    X.write_rb ~blk_id (get_rb())
      


  (** {2 The freelist} *)


type freelist_rt_blk = {
  min_free:blk_id;
} [@@deriving bin_io]
  

  
    
    
    
    


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
      let config = Lazy.force runtime_config in
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

  let config = Lazy.force runtime_config

  let _ : unit = lru#set_initial_state 
      (Lru_.init_state ~max_size:config.lru_max_size ~evict_count:config.lru_evict_count)

  let lru_ops = lru#get_lru_ops ()


  let store : (k,v,_) store =
    let module X = Kv_store_with_lru.Make(S) in
    let set_blk_dev_ops blk_dev_ops = X.blk_dev_ops:=
    
 failwith ""

end
*)
