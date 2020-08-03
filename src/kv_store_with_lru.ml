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

open Kv_intf

let runtime_config = Kv_config_runtime.config

type params = < 
  lru_params : < evict_count : int; max_size : int;>;
  pcache_blocks_limit : int; 
>

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

  type lru
  val lru_factory    : (k,v,lru,t) lru_factory

  val root_manager   : (blk_id,blk,t) Root_manager.root_manager

end

module Make(S:S) = struct
  module S=S
  open S

  let kvop_map_ops = pcache_factory#kvop_map_ops

  let ( >>= ) = monad_ops.bind

  let return = monad_ops.return

  module Btree_thread = Btree_thread.Make(S)

  module Pcache_thread = Pcache_thread.Make(S)

  module With_(A:sig
      val blk_dev_ops    : (blk_id,blk,t) blk_dev_ops
      val barrier        : unit -> (unit,t)m
      val sync           : unit -> (unit,t)m
      val freelist_ops   : (blk_id,t) freelist_ops_af
      val params         : params
    end) 
  = struct
    open A

    let root_man = root_manager#with_ ~blk_dev_ops

    let blk_alloc = freelist_ops

    let pc_with = pcache_factory#with_ ~blk_dev_ops ~barrier ~freelist_ops

(*
- (b_origin): pcache origin and btree root
- (b_pcache): empty pcache
- (b_empty_btree): empty btree
*)
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
      let lru_ref = ref @@ lru_factory#empty lru_params in
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
        method pcache_ops=pcache_ops (* raw! not with blks_limit etc *)
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
      let open Root_manager in
      let write_origin origin = root_man#write_origin ~blk_id:b_origin ~origin in

      (* btree *)
      btree_factory#uncached
        ~blk_dev_ops ~blk_alloc ~init_btree_root:origin.btree_root |> fun btree_o ->

      (* then pcache *)
      pc_with#restore ~hd:origin.pcache_origin.hd >>= fun pcache_ops ->

      (* then lru *)
      let to_lower msg = q_lru_pc#enqueue msg in
      let lru_ref = ref @@ lru_factory#empty lru_params in
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
        method pcache_ops=pcache_ops (* raw, not with limit *)
        method lru_ops=lru_ops
        method q_lru_pc=q_lru_pc
        method q_pc_bt=q_pc_bt
        method origin=b_origin
      end
      in
      return obj

    let obj = object
      method create=create
      method restore=restore
    end

  end (* With_ *)
  

  let with_
    ~blk_dev_ops
    ~barrier
    ~sync
    ~freelist_ops
    ~params
    =
    let open (struct
      module A = With_(struct
          let blk_dev_ops=blk_dev_ops
          let barrier=barrier
          let sync=sync
          let freelist_ops=freelist_ops
          let params=params
        end)
    end)
    in
    A.obj

  let kv_factory : _ kv_factory = object
    method with_=with_
  end

end (* Make *)


module Examples = struct
  module Int_int = struct
    module S1 = Shared_ctxt
    module S2 = struct
      include S1
      type k=int
      type v=int

      type kvop_map = Tjr_pcache.Make.Examples.Int_int.kvop_map
      let pcache_factory = Tjr_pcache.pcache_examples#for_int_int

      type node = Tjr_btree.Make_6.Examples.Int_int.node
      type leaf = Tjr_btree.Make_6.Examples.Int_int.leaf
      type dnode = (node, leaf) Isa_btree.dnode
      type ls = Tjr_btree.Make_6.Examples.Int_int.ls
      type blk = Tjr_fs_shared.ba_buf
      type wbc = Tjr_btree.Make_6.Examples.Int_int.wbc
      let btree_factory = Tjr_btree.btree_examples#int_int_factory

      type lru = Tjr_lru_cache.Lru_examples.Int_int.lru
      let lru_factory = Tjr_lru_cache.Lru_examples.Int_int.lru_factory
                          
      let root_manager = Root_manager.root_managers#for_lwt_ba_buf
    end

    module M = Make(S2)
    let kv_factory = M.kv_factory
  end
end


module Test() = struct
  (* open Tjr_monad.With_lwt *)
  open Shared_ctxt
  
  let kv_factory = Examples.Int_int.kv_factory

  let pcache_factory = Tjr_pcache.pcache_examples#for_int_int
  let plist_factory = pcache_factory#simple_plist_factory#plist_factory

  let params = 
    object 
      method lru_params=object method evict_count=10 method max_size=20 end 
      method pcache_blocks_limit=2
    end
  let count = 1000

  let test () = 
    blk_devs#lwt_open_file ~fn:"kv.store" ~create:true ~trunc:true >>= fun bd ->
    let blk_dev_ops=bd#blk_dev_ops in
    let root_man = Root_manager.root_managers#for_lwt_ba_buf#with_ ~blk_dev_ops in
    (* FIXME we should just have create and restore taking a fn *)
    shared_freelist#with_ ~fn:"freelist.store" |> fun fl -> 
    fl#create ~min_free:(B.of_int 0) >>= fun fl -> 
    let freelist_ops = fl#freelist_ops in
    let _ = freelist_ops in
    let barrier = (fun () -> return ()) in
    let sync = barrier in
    kv_factory#with_ 
      ~blk_dev_ops 
      ~barrier
      ~sync
      (* $(FIXME("""standardize on blk_alloc and blk_free; alter shared_freelist """)) *)
      ~freelist_ops:{blk_alloc=freelist_ops.Shared_freelist.alloc;blk_free=freelist_ops.free}
      ~params
    |> fun kv -> 
    kv#create () >>= fun kv_store -> 
    Printf.printf "%s: created from disk\n%!" __FILE__;
    let lru_ops = kv_store#lru_ops in
    let { mt_find; mt_insert; mt_delete; mt_sync_all_keys; _ } = lru_ops in
    (* NOTE kv_store.origin is the blk to restore from *)
    (* $(FIXME("""start threads automatically rather than requiring explicit start""")) *)
    Printf.printf "%s: starting btree thread\n%!" __FILE__;
    kv_store#btree_thread#start_btree_thread () >>= fun () ->
    Printf.printf "%s: started btree thread\n%!" __FILE__;
    kv_store#pcache_thread#start_pcache_thread () >>= fun () ->
    Printf.printf "%s: started pcache thread\n%!" __FILE__;
    (* perform some operations *)
    Printf.printf "%s: performing operations\n%!" __FILE__;
    0 |> iter_k (fun ~k n ->
        match n >= count with
        | true -> return ()
        | false -> 
          Printf.printf "%s: inserting %d\n%!" __FILE__ n;
          (* FIXME mt_insert should take an optional persist mode *)
          mt_insert n (2*n) >>= fun () ->
          k (n+1)) >>= fun () ->
    (* FIXME need some mechanism to free up all resources for KV store
       (GC may not be effective?) *)
    Printf.printf "%s: syncing keys\n%!" __FILE__;
    mt_sync_all_keys () >>= fun () ->
    Printf.printf "%s: syncing pcache\n%!" __FILE__;
    kv_store#pcache_ops.Pcache_ops.pcache_sync () >>= fun () ->
    (* FIXME add a sync to kv which calls the Lru, pcache (not btree- it is uncached) *)
    (* Writing root *)
    
    (* root_man#write_origin ~blk_id:(B.of_int 0) ~origin:Root_manager.{} *)
    Printf.printf "%s: closing\n%!" __FILE__;
    fl#close () >>= fun () ->
    Printf.printf "%s: pausing\n%!" __FILE__;
    Tjr_monad.With_lwt.(from_lwt @@ sleep 2.0) >>= fun () ->
    (* FIXME expose pcache ops and btree ops in kv_store *)
    (* bd#close () >>= fun () -> *)
    let blk_id = kv_store#origin in
    
    
    (* print out the contents of the pcache, for debugging *)
    root_man#read_origin blk_id >>= fun origin -> 
    Printf.printf "%s: read origin %d %d\n%!" __FILE__ (origin.pcache_origin.hd|>B.to_int) (origin.btree_root|>B.to_int);
    let plf = plist_factory#with_blk_dev_ops ~blk_dev_ops ~barrier in
    
    plf#init#read_from_hd origin.pcache_origin.hd >>= fun xs -> 
    xs |> List.map (fun (xs,nxt) -> xs) |> List.concat |> List.map Kvop.ii_op2s |> String.concat "," |> fun s -> Printf.printf "\n%s: plist is %s\n\n%!" __FILE__ s;

    kv#restore blk_id >>= fun kv2 -> 
    kv2#btree_thread#start_btree_thread () >>= fun () ->
    kv2#pcache_thread#start_pcache_thread () >>= fun () ->
    Printf.printf "%s: restored from disk\n%!" __FILE__;
    (* FIXME check the contents of the restored store *)
    0 |> iter_k (fun ~k n -> 
        match n >= count with
        | true -> return ()
        | false -> 
          Printf.printf "%s: finding %d\n%!" __FILE__ n;
          (* FIXME mt_insert should take an optional persist mode *)
          kv2#lru_ops.mt_find n >>= fun v ->
          assert(
            let b1 = v<>None in
            let b2 = b1 && (dest_Some v = 2*n) in            
            match b1&&b2 with 
            | true -> true
            | false -> 
              Printf.printf "%s: find did not return the correct result: %d %b %b\n%!" __FILE__ n b1 b2;
              false);
          k (n+1)) >>= fun () ->
    bd#close () >>= fun () -> 
    return ()

end
