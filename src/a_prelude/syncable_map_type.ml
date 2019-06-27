

(** A syncable map API.

Sync operations:

- sync to sync the whole map to disk
- ksync to sync a particular key
- ks_sync to sync a set of keys

*)
type ('k,'v,'ptr,'t) syncable_map = {
  find: 'k -> ('v option,'t)m;
  insert: 'k -> 'v -> (unit,'t)m;
  delete: 'k -> (unit,'t)m;
  sync: unit -> ('ptr,'t)m;
  ksync: 'k -> ('ptr,'t)m;
  ks_sync: 'k list -> ('ptr,'t)m
}
