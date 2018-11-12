open Tjr_monad.Types

type ('k,'v,'blkid,'t) btree_ops = {
  find: 'k -> ('v option,'t)m;
  insert: 'k -> 'v -> (unit,'t)m;
  delete: 'k -> (unit,'t)m;
  sync: unit -> ('blkid,'t)m;
}
