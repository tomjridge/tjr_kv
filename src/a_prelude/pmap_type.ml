

(** A persistent map is formed from a B-tree and a detachable map
   (dmap).

The "root" is a pair of the root for the persistent map and the root
   for the B-tree.

When a pmap is constructed, we expect that the implementation requires
   some way to handle the dmap detach event followed by the B-tree
   rollup and the creation of the new root pair.


*)


type ('k,'v,'t) pmap = {
  find: 'k -> ('v option,'t) m;
  insert: 'k -> 'v -> (unit,'t) m;
  delete: 'k -> (unit,'t)m;
}
