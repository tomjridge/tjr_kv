(** A key-value store (example instance) *)

(** See {!Kv_store_with_lru} for more details *)

(** {2 Main interfaces} *)

module Kv_intf = Kv_intf
(* include Kv_intf *)

module Intf_v2 = Intf_v2

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



