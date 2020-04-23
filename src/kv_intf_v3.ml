(** Interfaces based on a single "store" object with mutable refs.


NOTE the class types are not rendered well with odoc, so we reproduce them here.

{%html:

<div class="highlight" style="background: #f8f8f8"><pre style="line-height: 125%"><span></span><span style="color: #008000; font-weight: bold">sig</span>
  <span style="color: #008000; font-weight: bold">class</span> <span style="color: #008000; font-weight: bold">type</span> <span style="color: #666666">[</span><span style="color: #008000; font-weight: bold">&#39;</span>a<span style="color: #666666">]</span> mrshl <span style="color: #666666">=</span>
    <span style="color: #008000; font-weight: bold">object</span>
      <span style="color: #008000; font-weight: bold">method</span> of_blk <span style="color: #666666">:</span> <span style="color: #0000FF; font-weight: bold">Tjr_fs_shared</span>.<span style="color: #0000FF; font-weight: bold">Std_types</span>.blk <span style="color: #666666">-&gt;</span> <span style="color: #008000; font-weight: bold">&#39;</span>a
      <span style="color: #008000; font-weight: bold">method</span> to_blk <span style="color: #666666">:</span> <span style="color: #008000; font-weight: bold">&#39;</span>a <span style="color: #666666">-&gt;</span> <span style="color: #0000FF; font-weight: bold">Tjr_fs_shared</span>.<span style="color: #0000FF; font-weight: bold">Std_types</span>.blk
    <span style="color: #008000; font-weight: bold">end</span>
  <span style="color: #008000; font-weight: bold">class</span> <span style="color: #666666">[</span><span style="color: #008000; font-weight: bold">&#39;</span>a<span style="color: #666666">]</span> set_once <span style="color: #666666">:</span>
    <span style="color: #008000; font-weight: bold">object</span>
      <span style="color: #008000; font-weight: bold">val</span> <span style="color: #008000; font-weight: bold">mutable</span> the_ref <span style="color: #666666">:</span> <span style="color: #008000; font-weight: bold">&#39;</span>a option
      <span style="color: #008000; font-weight: bold">method</span> get <span style="color: #666666">:</span> <span style="color: #008000; font-weight: bold">&#39;</span>a
      <span style="color: #008000; font-weight: bold">method</span> set <span style="color: #666666">:</span> <span style="color: #008000; font-weight: bold">&#39;</span>a <span style="color: #666666">-&gt;</span> <span style="color: #B00040">unit</span>
    <span style="color: #008000; font-weight: bold">end</span>
  <span style="color: #008000; font-weight: bold">class</span> <span style="color: #008000; font-weight: bold">virtual</span> <span style="color: #666666">[</span><span style="color: #008000; font-weight: bold">&#39;</span>rt<span style="color: #666666">]</span> syncable <span style="color: #666666">:</span>
    <span style="color: #008000; font-weight: bold">object</span>
      <span style="color: #008000; font-weight: bold">val</span> blk_dev_ops <span style="color: #666666">:</span>
        <span style="color: #666666">(</span><span style="color: #0000FF; font-weight: bold">Tjr_fs_shared</span>.<span style="color: #0000FF; font-weight: bold">Std_types</span>.blk_id<span style="color: #666666">,</span> <span style="color: #0000FF; font-weight: bold">Tjr_fs_shared</span>.<span style="color: #0000FF; font-weight: bold">Std_types</span>.blk<span style="color: #666666">,</span>
         <span style="color: #0000FF; font-weight: bold">Tjr_fs_shared</span>.<span style="color: #0000FF; font-weight: bold">Std_types</span>.t<span style="color: #666666">)</span>
        <span style="color: #0000FF; font-weight: bold">Tjr_fs_shared</span>.blk_dev_ops set_once
      <span style="color: #008000; font-weight: bold">val</span> blk_id <span style="color: #666666">:</span> <span style="color: #0000FF; font-weight: bold">Tjr_fs_shared</span>.<span style="color: #0000FF; font-weight: bold">Std_types</span>.blk_id set_once
      <span style="color: #008000; font-weight: bold">val</span> marshaller <span style="color: #666666">:</span> <span style="color: #008000; font-weight: bold">&#39;</span>rt mrshl set_once
      <span style="color: #008000; font-weight: bold">val</span> rt <span style="color: #666666">:</span> <span style="color: #008000; font-weight: bold">&#39;</span>rt set_once
      <span style="color: #008000; font-weight: bold">method</span> sync_from_disk <span style="color: #666666">:</span>
        <span style="color: #B00040">unit</span> <span style="color: #666666">-&gt;</span> <span style="color: #666666">(</span><span style="color: #008000; font-weight: bold">&#39;</span>rt<span style="color: #666666">,</span> <span style="color: #0000FF; font-weight: bold">Tjr_fs_shared</span>.<span style="color: #0000FF; font-weight: bold">Std_types</span>.t<span style="color: #666666">)</span> <span style="color: #0000FF; font-weight: bold">Tjr_monad</span>.m
      <span style="color: #008000; font-weight: bold">method</span> sync_to_disk <span style="color: #666666">:</span>
        <span style="color: #B00040">unit</span> <span style="color: #666666">-&gt;</span> <span style="color: #666666">(</span><span style="color: #B00040">unit</span><span style="color: #666666">,</span> <span style="color: #0000FF; font-weight: bold">Tjr_fs_shared</span>.<span style="color: #0000FF; font-weight: bold">Std_types</span>.t<span style="color: #666666">)</span> <span style="color: #0000FF; font-weight: bold">Tjr_monad</span>.m
    <span style="color: #008000; font-weight: bold">end</span>
<span style="color: #008000; font-weight: bold">end</span>
</pre></div>

%}


 *)

open Tjr_monad.With_lwt
open Std_types

type rt_blk = {
  bt_rt:blk_id;
  pc_hd:blk_id;
  pc_tl:blk_id
}[@@deriving bin_io]


module Classes = struct
  class type ['a] mrshl = object
    method to_blk: 'a -> blk
    method of_blk: blk -> 'a
  end

  class ['a] set_once = object (self:'self)
    val mutable the_ref = (None:'a option)
    method set x =
      assert(the_ref=None);
      the_ref <- Some x
    method get = 
      assert(Option.is_some the_ref);
      Option.get the_ref
  end


  class virtual ['rt] syncable = object (self:'self)
    val blk_dev_ops : (blk_id,blk,t)blk_dev_ops set_once = new set_once

    val blk_id : blk_id set_once = new set_once

    val rt : 'rt set_once = new set_once

    val marshaller : 'rt mrshl set_once = new set_once

    method sync_to_disk () = 
      let blk_dev_ops,blk_id,rt,(m: 'rt mrshl) = 
        blk_dev_ops#get,blk_id#get,rt#get,marshaller#get
      in    
      blk_dev_ops.write ~blk_id ~blk:(m#to_blk rt)
    
    method sync_from_disk () = 
      let blk_dev_ops,blk_id,rt,(m: 'rt mrshl) = 
        blk_dev_ops#get,blk_id#get,rt#get,marshaller#get
      in    
      blk_dev_ops.read ~blk_id >>= fun blk ->
      return (m#of_blk blk)
  end
end

include Classes



(*
class type ['fl] freelist = object
  method set_blk_dev_ops: std_blk_dev_ops -> unit

  method set_rt : blk_id -> unit

  method set_freelist: 'fl -> unit

  method sync_to_disk: unit -> (unit,t)m

  method sync_from_disk: unit -> (unit,t)m
  
  method get_blk_allocator: unit -> (r,t)blk_allocator_ops
end


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
