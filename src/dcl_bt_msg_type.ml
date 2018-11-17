(** The type of messages sent from the DCL to the B-tree.

This is a callback-oriented interface, with operations [find] and
   [detach] (to handle a list of operations... assumed to come from a
   detach operation via a map (ie no duplicate keys)

*)
open Tjr_monad.Types
open Tjr_pcache.Ins_del_op_type  (* FIXME move to fs_shared *)
open Blk_id_type

type ('k,'v,'t) dcl_bt_msg = 
  | Find of 'k * ('v option -> (unit,'t) m)
  | Detach of {
      ops: ('k,'v) op list;
      new_dcl_root: blk_id
    }

