(** A key-value store (example instance) *)

(** See {!Kv_store_with_lru} for more details *)

(** {2 Main interfaces} *)

module Kv_intf = Kv_intf
include Kv_intf

module Intf_v2 = Intf_v2

(** {2 Configuration} *)

module Kv_runtime_config = Kv_runtime_config


(** {2 Lwt aux} *)

module Lwt_aux = Lwt_aux


(** {2 Root manager} *)

module Root_manager = Root_manager


(** {2 The key-value store} *)

module Kv_store_with_lru = Kv_store_with_lru


(** {2 Internal: profilers} *)

module Kv_profilers = Kv_profilers

