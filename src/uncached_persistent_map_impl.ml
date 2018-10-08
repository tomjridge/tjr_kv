(** An uncached kv store *)

(** 

NOTE see diagram in <root>/docs/

At a high level, this is a k -> v map.

This is uncached: there is no cache in front of the store. The pcache
works on a raw uncached block device. The B-tree syncs the block
device after every batch operation.

We expect to use this with an LRU in front.

At intervals, when the pcache becomes long, some initial prefix of the
pcache is rolled into the B-tree. The root of the pcache is adjusted
(the old blocks can be reclaimed), and the root is written to disk
(but not necessarily synced). On crash, if the old root is used there
is no problem - we just replay these modifications over the B-tree. We
require that if the new pcache root hits disk, the B-tree is also on
disk. One approach is to async (flush btree; flush pcache root).

*)

(* open Tjr_monad *)
open Tjr_monad.Monad
open Tjr_pcache

module type REQUIRES = sig 
  module Bt_blk_id:Tjr_int.TYPE_ISOMORPHIC_TO_INT
  module Pc_blk_id:Tjr_int.TYPE_ISOMORPHIC_TO_INT (* NOTE only needed for testing; otherwise abstract *)
end

module Make(Requires : REQUIRES) = struct
  
  open Requires
      
  open Tjr_btree.Map_ops

  open Persistent_log


  type bt_blk_id = Bt_blk_id.t
  type pc_blk_id = Pc_blk_id.t


  (** We execute a roll up in another thread, to avoid blocking the
     pcache. The roll-up thread will typically finish with a new pair
     of roots. *)
  type root_pair = {
    pcache_root: pc_blk_id;
    btree_root: bt_blk_id;
  }


  (** This should execute in another dedicated rollup thread, so that
     the pcache thread is not blocked. We can't just instantiate this
     function here. In the pcache thread we need to join a msg to the
     end of a msg queue, signalling the rollup thread to perform a
     rollup. In this case, the rollup thread likely already knows
     operations such as [bt_insert] etc. which it can use to
     instantiate this function.

      NOTE that we return a unit so that from the pcache we can call
     this function without waiting for the rollup to occur. So the new
     roots have to be handled somewhere else (not in the pcache
     thread).  *)
  let perform_rollup_in_rollup_thread
    ~monad_ops
    ~(bt_insert:'k -> 'v -> (unit,'t) m)
    ~bt_delete
    ~bt_sync (* to sync the B-tree to get the new B-tree root *)
    ~(kvop_map_bindings:'op_map -> ('k,'v) op list)
    ~sync_new_roots  (* not clear if we should just return the new roots rather than explicitly passing in a sync op *)
    =
    let ( >>= ) = monad_ops.bind in
    let return = monad_ops.return in
    let f ((old_root:'pc_blk_id),map,new_root) : (unit,'t) m =
      begin
        (* map consists of all the entries we need to roll up *)
        map |> kvop_map_bindings |> fun ops ->
        let rec loop ops = 
          match ops with
          | [] -> return (`Finished(old_root,new_root))
          | v::ops ->
            match v with
            | Insert (k,v) -> bt_insert k v >>= fun () -> loop ops
            | Delete k -> bt_delete k >>= fun () -> loop ops
        in
        loop ops
      end
      >>= 
      begin
        function `Finished(old_root,(*new*)pcache_root) -> 
          (* sync the btree *)
          bt_sync () >>= fun btree_root ->
          (* now we need to flush the new roots to disk *)
          sync_new_roots {pcache_root; btree_root} >>= fun () ->
          return ()
      end
    in
    f


  type ('k,'v,'t) ukv_ops = ('k,'v,'t) Tjr_btree.Map_ops.map_ops

  (* we perform a "roll up" operation, merging the pcache into the
     B-tree, when the number of pcache blocks reaches
     pcache_blocks_limit *)

  (** Construct the UKV. Parameters:
- [monad_ops]
- [pcache_ops]
- [pcache_blocks_limit]: how many blocks in the pcache before attempting a roll-up; if the length of pcache is [>=] this limit, we attempt a roll-up; NOTE that this limit should be >= 2 (if we roll up with 1 block, then in fact nothing gets rolled up because we roll up "upto" the current block; not a problem but probably pointless for testing)
- [bt_find]: called if key not in pcache map  FIXME do we need a write-through cache here? or just rely on the front-end LRU? FIXME note that even if a rollup is taking place, we can use the old B-tree root for the [bt_find] operation.
- [perform_rollup_in_rollup_thread]: called to detach the rollup into another thread; typically this operation puts a msg on a message queue which is then received and acted upon by the dedicated rollup thread
  *)
  let make_ukv_ops
      ~monad_ops 
      ~pcache_ops 
      ~pcache_blocks_limit 
      ~bt_find
      ~(perform_rollup_in_rollup_thread:('pc_blk_id * 'map * 'pc_blk_id) -> (unit,'t) m)
    : ('k,'v,'t) ukv_ops 
    =
    (* let open Mref_plus in *)
    let ( >>= ) = monad_ops.bind in
    let return = monad_ops.return in
    let (*Persistent_log.*){find; add; detach; get_block_list_length} = pcache_ops in
    let (pc_find,pc_add,pc_detach,pc_get_block_list_length) = (find,add,detach,get_block_list_length) in
    let find k = 
      pc_find k >>= fun op ->
      match op with
      | None -> bt_find k
      | Some op ->
        match op with
        | Insert(k,v) -> return (Some v)
        | Delete k -> return None
    in
    let maybe_roll_up () = 
      pc_get_block_list_length () >>= fun n ->
      match n >= pcache_blocks_limit with
      | false -> return `No_roll_up_needed
      | true -> 
        pc_detach () >>= fun (old_root,(map:'map),new_root,_(*new_map*)) ->
        perform_rollup_in_rollup_thread (old_root,map,new_root) >>= fun () ->
        return `Ok
    in
    let insert k v =
      pc_add (Insert(k,v)) >>= fun () -> 
      maybe_roll_up () >>= fun _ ->
      return ()
    in
    let delete k =
      pc_add (Delete k) >>= fun () -> 
      maybe_roll_up () >>= fun _ ->
      return ()
    in
    let insert_many k v kvs = 
      (* FIXME we should do something smarter here *)
      insert k v >>= fun () -> return kvs
    in
    (* NOTE returns a Tjr_btree.Map_ops.map_ops *)
    { find; insert; delete; insert_many }



  (* testing -------------------------------------------------------- *)

  module Test()  : sig val run_tests: depth:int -> unit end = struct

    open Tjr_monad
    open Monad
    open State_passing_instance
    module Spi = State_passing_instance

    (* 
We need
- monad; use state-passing
- btree_ops; these are just map ops
- pcache_ops; these are from persistent_log; the pcache state needs to be part of our global state
- ukv_mref_ops; this is just the state
- detach_map_ops; just a map supporting ('k, ('k, 'v) op, 'map) Tjr_map.map_ops 
- bt_sync: perhaps we also record valid states as part of the global state? the bt_sync operation could return a btree_root (iso to int); FIXME do we want to identify btree_root as a different type? or maybe work with 'a blkid?
- sync_ukv_roots: also record this as part of our global state


What do we want to test?

- That the ukv abstracts to a simple map
  - take B-tree state (as a map) and append pcache state (as a map)
  - the abstract state should be the same before and after a sync_ukv_root action (which occurs when a roll-up occurs)
- That the state is always well formed:
  - (concurrency ... needs to be tested with multiple insert threads and arb interleaving FIXME)
  - ?anything else?

*)


    (* FIXME remove "abstract" and just use model; remove model_ops *)
    module Pc_model = Persistent_log.Abstract_model_ops(Pc_blk_id)


    module K = Tjr_int.Make_type_isomorphic_to_int()
    type key = K.t

    module V = Tjr_int.Make_type_isomorphic_to_int()
    type value = V.t

    module K_map = Tjr_map.Make(struct type t = key let compare: t -> t -> int = Pervasives.compare end)

    type btree_repr = value K_map.Map_.t

    type pc_model_state = (key,value) Pc_model.state

    (* need a map from k to (k,v)op *)

    (* if we make the map types the same type, we can union the maps more easily *)
    type k_vop_map = (key,value)Persistent_log.op K_map.Map_.t

    type 'test state = {
      test:'test;
      (* pc_model_state: pc_model_state; *)
      btree_state: btree_repr;
      pcache_state: pc_model_state;  (* contains ptr_ref *)
      (* ukv_state: ukv_state; *)
      free_bt_blk_id: int;
      synced_btrees: (bt_blk_id* btree_repr)list;  (* assoc list *)
      synced_root_pairs: root_pair list; 
    }

    let init_test_state ~test = {
      test;
      btree_state=K_map.Map_.empty;
      pcache_state=Pc_model.{kvs=[];ptr_ref=Pc_blk_id.int2t 0};
      (* FIXME pcache root is duplicated *)
      (* ukv_state={ in_roll_up=false; pcache_root=Pc_blk_id.int2t 0; btree_root=Bt_blk_id.int2t 0 }; *)
      free_bt_blk_id=1;
      synced_btrees=[];
      synced_root_pairs=[]                     
    }


    let monad_ops: 'test state state_passing monad_ops =  
      Spi.monad_ops ()

    let ( >>= ) = monad_ops.bind 
    let return = monad_ops.return


    (* implement btree ops with a simple map; FIXME maybe put in tjr_btree.test *)
    let btree_ops : (key,value,'t) Tjr_btree.Map_ops.map_ops = 
      (* let ops = K_map.map_ops in *)
      let find k = with_world (fun s ->
          (try Some (K_map.Map_.find k s.btree_state) with _ -> None),s)
      in
      let insert k v = with_world (fun s ->
          (),{s with btree_state=K_map.Map_.add k v s.btree_state})
      in
      let delete k = with_world (fun s ->
          (),{s with btree_state=K_map.Map_.remove k s.btree_state})
      in
      (* FIXME could do better *)
      let insert_many k v kvs = insert k v >>= fun () -> return kvs in
      {find;insert;delete;insert_many}


    let map_union s1 s2 = 
      K_map.Map_.union (fun k a1 a2 -> Some a2) s1 s2 

(*
    (* merge bt and pc; for pc, we have ins and delete actions *)
    let map_merge bt pc =
      let f key v1 v2 =
        match v2 with
        | None -> v1
        | Some(Delete k') ->
          assert(key=k');
          None
        | Some(Insert (k,v)) ->
          assert(key=k);
          Some(v)
      in
      K_map.Map_.merge f bt pc
*)

    let ops_to_map ~init ~ops =
      Tjr_list.with_each_elt
        ~list:ops
        ~step:(fun ~state elt -> 
            match elt with 
            | Delete k ->  K_map.Map_.remove k state
            | Insert(k,v) -> K_map.Map_.add k v state)
        ~init

    let _ = ops_to_map

    let abstract_state s = 
      s.pcache_state.Pc_model.kvs |> List.map snd |> fun ops -> 
      let init = s.btree_state in
      ops_to_map ~init ~ops


    type abstract_state = value K_map.Map_.t


    let mref = Mref.{
        get=(fun () -> with_world (fun s -> s.pcache_state,s));
        set=fun pcache_state -> with_world (fun s -> (),{s with pcache_state})
      }

    (* we don't construct these; instead we use an abstract model
       FIXME the abstract model should be in plog *)
    let pcache_ops : (key, value, (key * (key, value) op) list, pc_blk_id, 'test state state_passing) plog_ops = 
      Pc_model.abstract_model_ops
        ~monad_ops
        ~ops_per_block:2
        ~mref

    let _ = pcache_ops

    let pcache_blocks_limit = 2

    (* let ukv_mref_ops : (ukv_state, 'test state state_passing) Mref_plus.mref = 
     *   let get () = with_world (fun s -> s.ukv_state,s) in
     *   let set ukv_state = with_world (fun s -> (),{s with ukv_state}) in
     *   let with_ref f = with_world (fun s -> 
     *       let (b,ukv_state) = f s.ukv_state in
     *       b,{s with ukv_state})
     *   in
     *   Mref_plus.{ get; set; with_ref } *)


    (* when we sync a btree we simply increment the bt_blk_id and
       store the current btree state in the list *)
    let bt_sync () : (bt_blk_id,'t) m = 
      with_world (fun s -> 
          let n = s.free_bt_blk_id in
          let nn = Bt_blk_id.int2t n in
          nn,{ s with free_bt_blk_id=n+1;
                      synced_btrees=(nn,s.btree_state)::s.synced_btrees })

    (* what do we want to do here? when we detach the pc, the abstract
       model increases ptr_ref; for each ptr ref, there is therefore a
       corresponding map from that ptr, and a map upto that ptr; when
       we update the bt and sync, it contains the map upto the ptr,
       and so does the pc; but then we swing the bt and pc root
       pointers with this operation; let's avoid tracking the
       correspondence between ptrs and kvops; instead, just require
       that after a sync the abstract map is what it should be FIXME
       we are not really testing the behaviour whereby pc and bt roots
       sync to disk at arbitrary times *)
    let sync_ukv_roots ~btree_root ~pcache_root = return ()

    (* FIXME why are we revealing the map impl type here? pcache_ops also shares 'map type *)
    let kvop_map_bindings kvops = kvops |> List.map snd

    let perform_rollup_in_rollup_thread =
      perform_rollup_in_rollup_thread
        ~monad_ops
        ~bt_insert:btree_ops.insert
        ~bt_delete:btree_ops.delete
        ~bt_sync
        ~kvop_map_bindings
        ~sync_new_roots:(fun root_pair -> 
            with_world (fun s ->
                ((),{s with synced_root_pairs=root_pair::s.synced_root_pairs})))

    let ukv_ops : (key,value,'t) ukv_ops = 
      make_ukv_ops
        ~monad_ops
        ~pcache_ops 
        ~pcache_blocks_limit 
        ~bt_find:btree_ops.find
        ~perform_rollup_in_rollup_thread

    let _ = pcache_ops

    let insert k v s =
      ukv_ops.insert k v |> fun m ->
      Tjr_monad.State_passing_instance.run ~init_state:s m |> fun (_,s') ->
      s'

    let delete k s =
      ukv_ops.delete k |> fun m ->
      Tjr_monad.State_passing_instance.run ~init_state:s m |> fun (_,s') ->
      s'


    (* OK; now we can run some tests by exhaustively enumerating states *)
    (* FIXME reuse btree testing code *)

    open Tjr_exhaustive_testing

    let ops = 
      Tjr_list.from_to 1 10 |> 
      List.map (fun k -> 
          let v = 2*k in
          let (k,v) = (K.int2t k, V.int2t v) in
          [Insert(k,v); Delete k]) |> 
      List.concat


    (* we do not have sets of states (since we can't really compare
       states for equality very easily); instead we just apply all
       operations upto a set depth *)


    (* the test state is a list of operations (most recent first) *)

    type op' = I of int * int | D of int  [@@deriving yojson]

    type ops' = op' list [@@deriving yojson]

    let op2op' = function
      | Insert(k,v) -> I(K.t2int k,V.t2int v)
      | Delete k -> D(K.t2int k)


    type iis = (int * int) list [@@deriving yojson]


    let run_tests ~depth =

      let step_test op s = {s with test=op::s.test} in

      let step s op = 
        if List.length s.test > depth then [] else
          begin 
            (* Printf.printf "stepping...\n%!"; *)
            match op with
            | Insert(k,v) -> [insert k v s|>step_test op]
            | Delete k -> [delete k s|>step_test op]
          end
      in

      let check_state s = () in

      let check_step s op s' = 
        let t = abstract_state s in
        match op with
        | Insert(k,v) ->
          let correct_t' = K_map.Map_.add k v t in
          let abstract_s' = abstract_state s' in
          Pcache_debug.log_lazy (fun () ->
              let bindings map = 
                K_map.Map_.bindings map |> 
                List.map (fun (k,v) -> K.t2int k,V.t2int v) |>
                iis_to_yojson |> Yojson.Safe.pretty_to_string
              in
              let expected = bindings correct_t' in
              let actual = bindings abstract_s' in
              Printf.sprintf "
Checking step...
Op: %s
Previous ops: %s
Expected: %s
Actual: %s
"
                (op|>op2op'|>op'_to_yojson|>Yojson.Safe.pretty_to_string)
                (s.test|>List.map op2op'|>ops'_to_yojson|>Yojson.Safe.pretty_to_string)
                expected
                actual
            );
          (* NOTE we can't just expect pervasive equality to work on maps *)
          assert(K_map.Map_.equal (=) correct_t' abstract_s');
          ()
        | Delete k ->
          let correct_t' = K_map.Map_.remove k t in
          let abstract_s' = abstract_state s' in
          Pcache_debug.log_lazy (fun () ->
              let bindings map = 
                K_map.Map_.bindings map |> 
                List.map (fun (k,v) -> K.t2int k,V.t2int v) |>
                iis_to_yojson |> Yojson.Safe.pretty_to_string
              in
              let expected = bindings correct_t' in
              let actual = bindings abstract_s' in
              Printf.sprintf "
Checking step...
Op: %s
Previous ops: %s
Expected: %s
Actual: %s
"
                (op|>op2op'|>op'_to_yojson|>Yojson.Safe.pretty_to_string)
                (s.test|>List.map op2op'|>ops'_to_yojson|>Yojson.Safe.pretty_to_string)
                expected
                actual
            );
          (* NOTE we can't just expect pervasive equality to work on maps *)
          assert(K_map.Map_.equal (=) correct_t' abstract_s');
          ()
      in

      let test_ops = { step; check_state; check_step } in


      let init_states = [init_test_state ~test:[]] in

      Printf.printf "%s: tests starting...\n%!" __FILE__;

      (* we also need to maintain a set of states; in this case, we
         can't really check equality of two states; so instead we need
         to impose a max bound on the number of states considered, which
         can be done by bounding the depth of operations *)
      assert(() = Tjr_exhaustive_testing.test_till_no_successor_states ~test_ops ~ops ~init_states);
      Printf.printf "%s: ...tests finished\n%!" __FILE__


  end  (* Test *)
end  (* Make *)
