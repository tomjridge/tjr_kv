open Tjr_monad.Types

(* FIXME also include "batch" op *)

(** The operations supported by the B-tree. Even though the B-tree is
   synchronous, the "sync" operation is used to reveal the current
   root block. *)
type ('k,'v,'blkid,'t) btree_ops = {
  find: 'k -> ('v option,'t)m;
  insert: 'k -> 'v -> (unit,'t)m;
  delete: 'k -> (unit,'t)m;
  sync: unit -> ('blkid,'t)m;  
}
