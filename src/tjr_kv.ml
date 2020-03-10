(** A key-value store (example instance) *)

(** See {!Kv_store_with_lru} for more details *)

(** {2 Main interfaces} *)

module Kv_intf = Kv_intf
(* include Kv_intf *)

module Intf_v2 = Intf_v2

(** {2 Configuration and profilers} *)

module Kv_conf_optcomp = Kv_conf_optcomp
module Kv_conf_runtime = Kv_conf_runtime
module Kv_conf_profilers = Kv_conf_profilers


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



