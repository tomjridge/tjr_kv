(** An in-mem map with root counter, for testing. *)
open Tjr_monad.Types
open Btree_ops

let empty_btree ~compare :('k,'v) Tjr_polymap.t = Tjr_polymap.empty compare

type ('btree,'t) with_btree = {
  with_btree: 
    'a. 
      (btree:'btree -> 
       set_btree:('btree -> (unit,'t)m)
       -> ('a,'t)m)
    -> ('a,'t)m
}



(* implement btree ops with a simple map; FIXME maybe put in tjr_btree.test *)
let btree_ops 
    ~monad_ops ~with_btree ~get_btree_root_and_incr 
  : ('k,'v,'blk_id,'t) btree_ops 
  = 
  (* let ( >>= ) = monad_ops.bind in *)
  let return = monad_ops.return in
  let with_btree = with_btree.with_btree in
  let find k = with_btree (fun ~btree ~set_btree -> 
      Tjr_polymap.find_opt k btree |> return)
  in
  let insert k v = with_btree (fun ~btree ~set_btree -> 
      Tjr_polymap.add k v btree |> set_btree)
  in
  let delete k = with_btree (fun ~btree ~set_btree -> 
      Tjr_polymap.remove k btree |> set_btree)
  in
  let sync () = get_btree_root_and_incr () in
  {find;insert;delete;sync}

