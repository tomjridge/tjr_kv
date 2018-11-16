(** Various message types *)


type ('k,'v,'t) lru_dcl_msg = ('k,'v,'t) Tjr_lru_cache.Msg_type.msg


(** Type of messages dcl -> btree *)
(* only one message - a rollup message FIXME replace with detach type
   from pcache *)
type ('blk_id,'map) dcl_bt_msg = {
  old_root: 'blk_id;
  new_root: 'blk_id;
  map: 'map;  (* of k -> v op *)
}




