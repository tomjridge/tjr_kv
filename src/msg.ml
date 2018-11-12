(** Various message types *)

module Q_dcl_btree = struct


  (** Type of messages dcl -> btree *)
  (* only one message - a rollup message *)
  type ('blk_id,'map) msg' = {
    old_root: 'blk_id;
    new_root: 'blk_id;
    map: 'map;  (* of k -> v op *)
  }
    

end


