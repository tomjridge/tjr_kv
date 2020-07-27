(** tjr_kv: A key-value store (example instance) *)

include Summary


module Kv_intf = Kv_intf


(** {2 Configuration and profilers} *)

module Kv_config_optcomp = Kv_config_optcomp
module Kv_config_runtime = Kv_config_runtime
module Kv_config_profilers = Kv_config_profilers


(* {2 Lwt aux} *)
(* module Lwt_aux = Lwt_aux *)


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
