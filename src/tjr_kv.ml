(** A key value store.

This is a key value store, using the following technologies:

- tjr_btree, a B-tree library
- tjr_pcache, a persistent cache
- an LRU write-back cache

 *)


open Tjr_pcache.Persistent_log
