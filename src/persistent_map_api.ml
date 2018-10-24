(** The API exposed by the KV store includes various sync commands, and various modes to invoke the standard map functionality. *)

open Tjr_monad.Monad

(** When calling a function, we can opt to wait for it to complete ([Block]) or not ([Continue]). *)
type blocking_mode = Continue | Block

(** We can opt for an operation such as insert to persist immediately
   to disk, or not ([Transient]). Transient operations can be
   persisted via sync calls, or they may persist asynchronously
   e.g. when a cache becomes full and entries much be flushed to
   disk. *)
type persistent_mode = Transient | Persistent of blocking_mode

type mode = persistent_mode

let default_mode = Transient
(** The default mode is [Transient]. *)

type ('k,'v,'t) pmap_ops = {
  find: 'k -> ('v option,'t) m;
  insert: mode -> 'k -> 'v -> (unit,'t) m;
  delete: mode -> 'k -> (unit,'t) m;
  sync_key: blocking_mode -> 'k -> (unit,'t) m;
  sync_all_keys: blocking_mode -> unit -> (unit,'t) m;
}