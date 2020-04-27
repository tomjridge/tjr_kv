(** A key-value store (example instance) 

{2 Architecture}

{%html: 

<img src="https://docs.google.com/drawings/d/e/2PACX-1vSnTmJGnVDyxnrBZ_VOVZ7T0O9etqZa-BDPu-EPH9ziiNjY375TMgO-ENB9UO4e-HT3qmtbJKvFOFl0/pub?w=453&amp;h=373">

<img src="https://docs.google.com/drawings/d/e/2PACX-1vTIXhyNa7dovQYXuJXBMwPQZU99-x_tRdTIH3SkMUDyPwbL31zExWXauT2hO-eRIUcnGP3RVHiSHrjt/pub?w=557&amp;h=428">

%}

Simplified interface:

{%html: 

<div class="highlight" style="background: #f8f8f8"><pre style="line-height: 125%"><span></span><span style="color: #008000; font-weight: bold">module</span> <span style="color: #008000; font-weight: bold">type</span> <span style="color: #0000FF; font-weight: bold">KV</span> <span style="color: #666666">=</span> <span style="color: #008000; font-weight: bold">sig</span>
  <span style="color: #008000; font-weight: bold">type</span> <span style="color: #666666">(</span><span style="color: #008000; font-weight: bold">&#39;</span>a<span style="color: #666666">,</span> <span style="color: #008000; font-weight: bold">&#39;</span>t<span style="color: #666666">)</span> m

  <span style="color: #008000; font-weight: bold">type</span> k

  <span style="color: #008000; font-weight: bold">type</span> v

  <span style="color: #008000; font-weight: bold">type</span> blk_id

  <span style="color: #008000; font-weight: bold">type</span> <span style="color: #666666">(</span><span style="color: #008000; font-weight: bold">&#39;</span>k<span style="color: #666666">,</span> <span style="color: #008000; font-weight: bold">&#39;</span>v<span style="color: #666666">)</span> kvop <span style="color: #666666">=</span> <span style="color: #0000FF; font-weight: bold">Insert</span> <span style="color: #008000; font-weight: bold">of</span> <span style="color: #008000; font-weight: bold">&#39;</span>k <span style="color: #666666">*</span> <span style="color: #008000; font-weight: bold">&#39;</span>v <span style="color: #666666">|</span> <span style="color: #0000FF; font-weight: bold">Delete</span> <span style="color: #008000; font-weight: bold">of</span> <span style="color: #008000; font-weight: bold">&#39;</span>k

  <span style="color: #408080; font-style: italic">(* NOTE this is the LRU entry type *)</span>
  <span style="color: #008000; font-weight: bold">type</span> dirty <span style="color: #666666">=</span> <span style="color: #B00040">bool</span>

  <span style="color: #008000; font-weight: bold">type</span> <span style="color: #008000; font-weight: bold">&#39;</span>v entry <span style="color: #666666">=</span>
    <span style="color: #666666">|</span> <span style="color: #0000FF; font-weight: bold">Insert</span> <span style="color: #008000; font-weight: bold">of</span> <span style="color: #666666">{</span> <span style="color: #008000; font-weight: bold">value</span> <span style="color: #666666">:</span> <span style="color: #008000; font-weight: bold">&#39;</span>v<span style="color: #666666">;</span> dirty <span style="color: #666666">:</span> dirty <span style="color: #666666">}</span>
    <span style="color: #666666">|</span> <span style="color: #0000FF; font-weight: bold">Delete</span> <span style="color: #008000; font-weight: bold">of</span> <span style="color: #666666">{</span> dirty <span style="color: #666666">:</span> dirty <span style="color: #666666">}</span>
    <span style="color: #666666">|</span> <span style="color: #0000FF; font-weight: bold">Lower</span> <span style="color: #008000; font-weight: bold">of</span> <span style="color: #008000; font-weight: bold">&#39;</span>v option

  <span style="color: #008000; font-weight: bold">type</span> <span style="color: #666666">(</span><span style="color: #008000; font-weight: bold">&#39;</span>k<span style="color: #666666">,</span> <span style="color: #008000; font-weight: bold">&#39;</span>v<span style="color: #666666">,</span> <span style="color: #008000; font-weight: bold">&#39;</span>blk_id<span style="color: #666666">,</span> <span style="color: #008000; font-weight: bold">&#39;</span>t<span style="color: #666666">)</span> btree_ops <span style="color: #666666">=</span> <span style="color: #666666">{</span>
    find <span style="color: #666666">:</span> <span style="color: #008000; font-weight: bold">&#39;</span>k <span style="color: #666666">-&gt;</span> <span style="color: #666666">(</span><span style="color: #008000; font-weight: bold">&#39;</span>v option<span style="color: #666666">,</span> <span style="color: #008000; font-weight: bold">&#39;</span>t<span style="color: #666666">)</span> m<span style="color: #666666">;</span>
    insert <span style="color: #666666">:</span> <span style="color: #008000; font-weight: bold">&#39;</span>k <span style="color: #666666">-&gt;</span> <span style="color: #008000; font-weight: bold">&#39;</span>v <span style="color: #666666">-&gt;</span> <span style="color: #666666">(</span><span style="color: #B00040">unit</span><span style="color: #666666">,</span> <span style="color: #008000; font-weight: bold">&#39;</span>t<span style="color: #666666">)</span> m<span style="color: #666666">;</span>
    delete <span style="color: #666666">:</span> <span style="color: #008000; font-weight: bold">&#39;</span>k <span style="color: #666666">-&gt;</span> <span style="color: #666666">(</span><span style="color: #B00040">unit</span><span style="color: #666666">,</span> <span style="color: #008000; font-weight: bold">&#39;</span>t<span style="color: #666666">)</span> m<span style="color: #666666">;</span>
    sync <span style="color: #666666">:</span> <span style="color: #B00040">unit</span> <span style="color: #666666">-&gt;</span> <span style="color: #666666">(</span><span style="color: #008000; font-weight: bold">&#39;</span>blk_id<span style="color: #666666">,</span> <span style="color: #008000; font-weight: bold">&#39;</span>t<span style="color: #666666">)</span> m<span style="color: #666666">;</span>
  <span style="color: #666666">}</span>

  <span style="color: #008000; font-weight: bold">type</span> <span style="color: #666666">(</span><span style="color: #008000; font-weight: bold">&#39;</span>k<span style="color: #666666">,</span> <span style="color: #008000; font-weight: bold">&#39;</span>v<span style="color: #666666">,</span> <span style="color: #008000; font-weight: bold">&#39;</span>blk_id<span style="color: #666666">,</span> <span style="color: #008000; font-weight: bold">&#39;</span>t<span style="color: #666666">)</span> pc_bt_msg <span style="color: #666666">=</span>
    <span style="color: #666666">|</span> <span style="color: #0000FF; font-weight: bold">Find</span> <span style="color: #008000; font-weight: bold">of</span> <span style="color: #008000; font-weight: bold">&#39;</span>k <span style="color: #666666">*</span> <span style="color: #666666">(</span><span style="color: #008000; font-weight: bold">&#39;</span>v option <span style="color: #666666">-&gt;</span> <span style="color: #666666">(</span><span style="color: #B00040">unit</span><span style="color: #666666">,</span> <span style="color: #008000; font-weight: bold">&#39;</span>t<span style="color: #666666">)</span> m<span style="color: #666666">)</span>
    <span style="color: #666666">|</span> <span style="color: #0000FF; font-weight: bold">Detach</span> <span style="color: #008000; font-weight: bold">of</span> <span style="color: #666666">{</span> ops <span style="color: #666666">:</span> <span style="color: #666666">(</span><span style="color: #008000; font-weight: bold">&#39;</span>k<span style="color: #666666">,</span> <span style="color: #008000; font-weight: bold">&#39;</span>v<span style="color: #666666">)</span> kvop <span style="color: #B00040">list</span><span style="color: #666666">;</span> new_pcache_root <span style="color: #666666">:</span> <span style="color: #008000; font-weight: bold">&#39;</span>blk_id <span style="color: #666666">}</span>

  <span style="color: #008000; font-weight: bold">type</span> <span style="color: #666666">(</span><span style="color: #008000; font-weight: bold">&#39;</span>k<span style="color: #666666">,</span> <span style="color: #008000; font-weight: bold">&#39;</span>v<span style="color: #666666">,</span> <span style="color: #008000; font-weight: bold">&#39;</span>t<span style="color: #666666">)</span> lru_pc_msg <span style="color: #666666">=</span>
    <span style="color: #666666">|</span> <span style="color: #0000FF; font-weight: bold">Insert</span> <span style="color: #008000; font-weight: bold">of</span> <span style="color: #008000; font-weight: bold">&#39;</span>k <span style="color: #666666">*</span> <span style="color: #008000; font-weight: bold">&#39;</span>v <span style="color: #666666">*</span> <span style="color: #666666">(</span><span style="color: #B00040">unit</span> <span style="color: #666666">-&gt;</span> <span style="color: #666666">(</span><span style="color: #B00040">unit</span><span style="color: #666666">,</span> <span style="color: #008000; font-weight: bold">&#39;</span>t<span style="color: #666666">)</span> m<span style="color: #666666">)</span>
    <span style="color: #666666">|</span> <span style="color: #0000FF; font-weight: bold">Delete</span> <span style="color: #008000; font-weight: bold">of</span> <span style="color: #008000; font-weight: bold">&#39;</span>k <span style="color: #666666">*</span> <span style="color: #666666">(</span><span style="color: #B00040">unit</span> <span style="color: #666666">-&gt;</span> <span style="color: #666666">(</span><span style="color: #B00040">unit</span><span style="color: #666666">,</span> <span style="color: #008000; font-weight: bold">&#39;</span>t<span style="color: #666666">)</span> m<span style="color: #666666">)</span>
    <span style="color: #666666">|</span> <span style="color: #0000FF; font-weight: bold">Find</span> <span style="color: #008000; font-weight: bold">of</span> <span style="color: #008000; font-weight: bold">&#39;</span>k <span style="color: #666666">*</span> <span style="color: #666666">(</span><span style="color: #008000; font-weight: bold">&#39;</span>v option <span style="color: #666666">-&gt;</span> <span style="color: #666666">(</span><span style="color: #B00040">unit</span><span style="color: #666666">,</span> <span style="color: #008000; font-weight: bold">&#39;</span>t<span style="color: #666666">)</span> m<span style="color: #666666">)</span>
    <span style="color: #666666">|</span> <span style="color: #0000FF; font-weight: bold">Evictees</span> <span style="color: #008000; font-weight: bold">of</span> <span style="color: #666666">(</span><span style="color: #008000; font-weight: bold">&#39;</span>k <span style="color: #666666">*</span> <span style="color: #008000; font-weight: bold">&#39;</span>v entry<span style="color: #666666">)</span> <span style="color: #B00040">list</span>
        <span style="color: #408080; font-style: italic">(** NOTE this is the same as LRU msg type *)</span>

  <span style="color: #408080; font-style: italic">(* NOTE the interface provided by the key-value store is the same as</span>
<span style="color: #408080; font-style: italic">     that of the LRU component, which we reproduce here *)</span>
  <span style="color: #008000; font-weight: bold">type</span> persist_mode <span style="color: #666666">=</span> <span style="color: #0000FF; font-weight: bold">Persist_later</span> <span style="color: #666666">|</span> <span style="color: #0000FF; font-weight: bold">Persist_now</span>

  <span style="color: #008000; font-weight: bold">type</span> <span style="color: #666666">(</span><span style="color: #008000; font-weight: bold">&#39;</span>k<span style="color: #666666">,</span> <span style="color: #008000; font-weight: bold">&#39;</span>v<span style="color: #666666">,</span> <span style="color: #008000; font-weight: bold">&#39;</span>t<span style="color: #666666">)</span> mt_ops <span style="color: #666666">=</span> <span style="color: #666666">{</span>
    mt_find <span style="color: #666666">:</span> <span style="color: #008000; font-weight: bold">&#39;</span>k <span style="color: #666666">-&gt;</span> <span style="color: #666666">(</span><span style="color: #008000; font-weight: bold">&#39;</span>v option<span style="color: #666666">,</span> <span style="color: #008000; font-weight: bold">&#39;</span>t<span style="color: #666666">)</span> m<span style="color: #666666">;</span>
    mt_insert <span style="color: #666666">:</span> persist_mode <span style="color: #666666">-&gt;</span> <span style="color: #008000; font-weight: bold">&#39;</span>k <span style="color: #666666">-&gt;</span> <span style="color: #008000; font-weight: bold">&#39;</span>v <span style="color: #666666">-&gt;</span> <span style="color: #666666">(</span><span style="color: #B00040">unit</span><span style="color: #666666">,</span> <span style="color: #008000; font-weight: bold">&#39;</span>t<span style="color: #666666">)</span> m<span style="color: #666666">;</span>
    mt_delete <span style="color: #666666">:</span> persist_mode <span style="color: #666666">-&gt;</span> <span style="color: #008000; font-weight: bold">&#39;</span>k <span style="color: #666666">-&gt;</span> <span style="color: #666666">(</span><span style="color: #B00040">unit</span><span style="color: #666666">,</span> <span style="color: #008000; font-weight: bold">&#39;</span>t<span style="color: #666666">)</span> m<span style="color: #666666">;</span>
    mt_sync_key <span style="color: #666666">:</span> <span style="color: #008000; font-weight: bold">&#39;</span>k <span style="color: #666666">-&gt;</span> <span style="color: #666666">(</span><span style="color: #B00040">unit</span><span style="color: #666666">,</span> <span style="color: #008000; font-weight: bold">&#39;</span>t<span style="color: #666666">)</span> m<span style="color: #666666">;</span>
    mt_sync_all_keys <span style="color: #666666">:</span> <span style="color: #B00040">unit</span> <span style="color: #666666">-&gt;</span> <span style="color: #666666">(</span><span style="color: #B00040">unit</span><span style="color: #666666">,</span> <span style="color: #008000; font-weight: bold">&#39;</span>t<span style="color: #666666">)</span> m<span style="color: #666666">;</span>
  <span style="color: #666666">}</span>
<span style="color: #008000; font-weight: bold">end</span>
</pre></div>

%}

*)

(** See {!Kv_store_with_lru} for more details *)


(** {2 Main interfaces} *)

module Kv_intf = Kv_intf
(* include Kv_intf *)

module Kv_intf_v2 = Kv_intf_v2

(* module Kv_intf_v3 = Kv_intf_v3 *)


(** {2 Configuration and profilers} *)

module Kv_config_optcomp = Kv_config_optcomp
module Kv_config_runtime = Kv_config_runtime
module Kv_config_profilers = Kv_config_profilers


(** {2 Lwt aux} *)

module Lwt_aux = Lwt_aux


(** {2 Root manager} *)

module Root_manager = Root_manager


(** {2 Btree thread} *)

module Btree_thread = Btree_thread



(** {2 Pcache thread} *)

module Pcache_thread = Pcache_thread


(** {2 Lru} *)

module Lru = Lru


(** {2 The key-value store} *)

module Kv_store_with_lru = Kv_store_with_lru



(** {2 Further notes} *)

(**

{3 Combining B-tree and pcache roots in a single block}

One option when syncing the btree+pcache combination would be to write
   the pcache roots to disk, and then (in another block) write the
   B-tree root. This is fine, but if a crash occurs inbetween, we have
   to recover (which isn't difficult, but still adds complexity). As
   an alternative, we can write the btree and the pcache roots into
   the same block atomically. This means that we don't have to worry
   about recovering from a crash (this approach is crash safe by
   design).

*)
