(** Interfaces based on a single "store" object with mutable refs.


NOTE the class types are not rendered well with odoc.

{[
  class type ['a] mrshlr = object
    method to_blk: 'a -> blk
    method of_blk: blk -> 'a
  end

  class type ['a] set_once = object
    method set: 'a -> unit
    method get: unit -> 'a
    method is_set: bool
  end

  class type ['a] mutable_ = object
    method set: 'a -> unit
    method get: unit -> 'a
    method is_set: bool
  end

  (** This is a container for a type 'a that can be stored in a single
     block. The initialized method checks that all the state parts of
     a component are initialized; use with assert *)
  class type ['a] on_disk_block = object
    val blk_dev_ops    : (blk_id,blk,t)blk_dev_ops set_once
    val blk_id         : blk_id set_once
    val contents       : 'a mutable_
    val marshaller     : 'a mrshlr set_once
    method sync_to_disk   : unit -> (unit,t)m
    method sync_from_disk : unit -> (unit,t)m
    method is_initialized : bool
  end

  class type ['fl] freelist = object
    inherit ['fl] on_disk_block
    method blk_allocator: (r,t)blk_allocator_ops
  end

  type t1 = { 
    mutable bt_rt:blk_id;
    mutable pc_hd:blk_id; 
    mutable pc_tl:blk_id 
  }[@@deriving bin_io]

  class type root_man = object
    inherit [t1] on_disk_block
  end

  (* we expect that, once setup, the threads run forever; other
     applications may need to shutdown threads instead. *) 
  class type btree = object
    method blk_dev_ops    : (blk_id,blk,t)blk_dev_ops set_once
    method blk_id         : blk_id mutable_ (* root of the btree *)
    method thread         : < start_thread: unit -> (unit,t)m >
    method btree_ops      : unit -> ('k,'v,blk_id,t) Kv_intf.Btree_ops.btree_ops
    method is_initialized : bool

    (** Flush the write back cache, after merging a prefix of the pcache *)
    method flush_cache    : unit -> (unit,t)m
  end

  class type ['kvop_map] pcache = object
    method blk_dev_ops    : (blk_id,blk,t)blk_dev_ops set_once
    method pcache_state   : (r,'kvop_map) Tjr_pcache.Pcache_state.pcache_state mutable_
    method thread         : < start_thread: unit -> (unit,t)m >
    method sync_to_disk   : unit -> (unit,t)m
    method sync_from_disk : hd:blk_id -> tl:blk_id -> (unit,t)m
    method is_initialized : bool
  end

  class type ['k,'v,'mt_state] lru = object
    method get_lru_state: 'mt_state mutable_
    method lru_ops: unit -> ('k,'v,t)mt_ops
  end
  
  (** A collection of all the stateful components *)
  class type ['k,'v,'fl,'kvop_map,'mt_state] kv_store = object
    method root_man : root_man 
    method btree    : btree
    method q_pc_bt  : ('k,'v) Kv_intf_v2.q_pc_bt
    method pcache   : 'kvop_map pcache
    method q_lru_pc : ('k,'v) Kv_intf_v2.q_lru_pc
    method lru      : ('k,'v,'mt_state) lru

    (** Checks all components are initialized *)
    method is_initialized : bool
  end
]}

 *)

[@@@ warning "-33"]
open Tjr_monad.With_lwt
open Std_types

module Pvt_class_types = struct

  (** {2 Generic syncable object} *)

  class type ['a] mrshlr = object
    method to_blk: 'a -> blk
    method of_blk: blk -> 'a
  end

  class type ['a] set_once = object
    method set: 'a -> unit
    method get: unit -> 'a
    method is_set: bool
  end

  class type ['a] mutable_ = object
    method set: 'a -> unit
    method get: unit -> 'a
    method is_set: bool
  end

  (** This is a container for a type 'a that can be stored in a single
     block. The initialized method checks that all the state parts of
     a component are initialized; use with assert *)
  class type ['a] on_disk_block = object
    val blk_dev_ops    : (blk_id,blk,t)blk_dev_ops set_once
    val blk_id         : blk_id set_once
    val contents       : 'a mutable_
    val marshaller     : 'a mrshlr set_once
    method sync_to_disk   : unit -> (unit,t)m
    method sync_from_disk : unit -> (unit,t)m
    method is_initialized : bool
  end

  class type ['fl] freelist = object
    inherit ['fl] on_disk_block
    method blk_allocator: (r,t)blk_allocator_ops
  end

  type t1 = { 
    mutable bt_rt:blk_id;
    mutable pc_hd:blk_id; 
    mutable pc_tl:blk_id 
  }[@@deriving bin_io]

  class type root_man = object
    inherit [t1] on_disk_block
  end

  (* we expect that, once setup, the threads run forever; other
     applications may need to shutdown threads instead. *) 
  class type btree = object
    method blk_dev_ops    : (blk_id,blk,t)blk_dev_ops set_once
    method blk_id         : blk_id mutable_ (* root of the btree *)
    method thread         : < start_thread: unit -> (unit,t)m >
    method btree_ops      : unit -> ('k,'v,blk_id,t) Kv_intf.Btree_ops.btree_ops
    method is_initialized : bool

    (** Flush the write back cache, after merging a prefix of the pcache *)
    method flush_cache    : unit -> (unit,t)m
  end

  class type ['kvop_map] pcache = object
    method blk_dev_ops    : (blk_id,blk,t)blk_dev_ops set_once
    method pcache_state   : (r,'kvop_map) Tjr_pcache.Pcache_state.pcache_state mutable_
    method thread         : < start_thread: unit -> (unit,t)m >
    method sync_to_disk   : unit -> (unit,t)m
    method sync_from_disk : hd:blk_id -> tl:blk_id -> (unit,t)m
    method is_initialized : bool
  end

  class type ['k,'v,'mt_state] lru = object
    method get_lru_state: 'mt_state mutable_
    method lru_ops: unit -> ('k,'v,t)mt_ops
  end
  
  (** A collection of all the stateful components *)
  class type ['k,'v,'fl,'kvop_map,'mt_state] kv_store = object
    method root_man : root_man 
    method btree    : btree
    method q_pc_bt  : ('k,'v) Kv_intf_v2.q_pc_bt
    method pcache   : 'kvop_map pcache
    method q_lru_pc : ('k,'v) Kv_intf_v2.q_lru_pc
    method lru      : ('k,'v,'mt_state) lru

    (** Checks all components are initialized *)
    method is_initialized : bool
  end

end

module Classes = struct
  
  module P = Pvt_class_types

  (** {2 Generic syncable object} *)

  (* Convert a type with binprot to a marshaller *)
  module Make_mrshlr(S:sig type tt[@@deriving bin_io] end) = struct
    open S

    let to_blk (x:tt) = 
      let buf = buf_create () in
      let _ : int = bin_write_tt buf ~pos:0 x in
      buf

    let of_blk blk = 
      bin_read_tt blk ~pos_ref:(ref 0)
      
    let mrshlr : tt P.mrshlr = object
      method to_blk = to_blk
      method of_blk = of_blk        
    end 

  end

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

  class ['a] on_disk_block = object (self:'self)
    val blk_dev_ops = (new set_once:(blk_id,blk,t)blk_dev_ops set_once)
    val blk_id = (new set_once:blk_id set_once)
    val contents = (new mutable_ : 'a mutable_)
    val marshaller = (new set_once:'a P.mrshlr set_once)
    method sync_to_disk () = 
      blk_dev_ops#get.write ~blk_id:(blk_id#get) ~blk:(marshaller#get#to_blk contents#get)
    method sync_from_disk () = 
      blk_dev_ops#get.read ~blk_id:(blk_id#get) >>= fun blk ->
      contents#set (marshaller#get#of_blk blk); return ()
    method is_initialized = blk_dev_ops#is_set && blk_id#is_set && contents#is_set
  end
  
  (* FIXME how to check that this class definition satisfies the class type definition? *)
  let f (x:'a on_disk_block) : 'a P.on_disk_block = x


(*
  (** {2 Freelist} *)

  module Freelist = struct

    type x = { mutable min_free_blk_id:blk_id }[@@deriving bin_io]

    class freelist = object
      inherit [x] syncable

      val pvt_blk_allocator : (r,t)blk_allocator_ops set_once = new set_once

      method get_blk_allocator () = 
        match pvt_blk_allocator#is_set () with
        | true -> pvt_blk_allocator#get
        | false -> 
          rt#get |> fun rb -> 
          let blk_alloc () = 
            let min = rb.min_free_blk_id in
            rb.min_free_blk_id <- B.inc rb.min_free_blk_id;
            return min
          in
          let blk_free blk_id = return () in
          { blk_alloc; blk_free }     

      initializer 
        let module M = Make_mrshlr(struct type tt = x[@@deriving bin_io] end) in
        marshaller#set M.mrshlr
    end
  end
  class freelist = Freelist.freelist



  (** {2 Root manager, for B-tree and pcache roots} *)

  module Root_man = struct
    
    type x = { 
      mutable bt_rt:blk_id;
      mutable pc_hd:blk_id; 
      mutable pc_tl:blk_id 
    }[@@deriving bin_io]

    class root_man = object
      val sync_ops : x syncable = new syncable
      method get_sync_ops = sync_ops

      initializer 
        let module M = Make_mrshlr(struct type tt = x[@@deriving bin_io] end) in
        sync_ops#set_marshaller M.mrshlr
    end

  end
  class root_man = Root_man.root_man


  (** {2 B-tree} *)

  module Btree = struct
    
    class btree = object (self)
      method f = self#get_blk_dev_ops |> fun (x:int) -> x

  end
  

  (** {2 The kv store, as a collection of components} *)

  module Kv_store = struct
    open Kv_intf_v2
    class ['k,'v] kv_store = object
      val root_man = new root_man
      val btree = ()
      val q_pc_bt : ('k,'v) q_pc_bt = failwith ""
      val pc = ()
      val q_lru_pc : ('k,'v)q_lru_pc = failwith ""
      method set_blk_dev_ops x = 
        root_man#get_sync_ops#set_blk_dev_ops x;
        (* btree#set_blk_dev_ops x; *)
        ()
          
    end
  end

*)
  
end

include Classes


(*


class type ['k, 'v, 'kvop_map, 'fl] store = object

  method set_blk_dev_ops: std_blk_dev_ops -> unit

  method set_rt_blk_id : blk_id -> unit

  (* This doesn't sync to disk *)
  method set_rt_blk: rt_blk -> unit

  method sync_rt_blk_from_disk: unit -> (unit,t)m

  method sync_rt_blk_to_disk: unit -> (unit,t)m
  (* method write_rt_blk: rt_blk -> (unit,t)m *)


  (* this also contains the blk allocator *)
  method set_freelist: 'fl freelist -> unit



  (* this starts the btree thread *)
  method boot_btree: unit -> (unit,t)m

  method get_btree: unit -> ('k,'v,r,t) Kv_intf.Btree_ops.btree_ops

  method get_btree_cache: unit (* FIXME *)


  method boot_pcache: unit -> (unit,t)m
      
  method get_pcache: unit -> (r,'kvop_map)Pcache_state.pcache_state

      
  (* method boot_lru: unit -> (unit,t)m *)

  method get_lru: unit -> ('k,'v,t)mt_ops
  (* and various kinds of marshallers? empty_leaf as blk? *)
end


*)
