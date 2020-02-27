(** This is a pcache (detachable map) which automatically detaches after
   a certain number of blocks *)

type ('k,'v,'t) pcache_with_lim_ops = ('k,'v,'t) Tjr_fs_shared.Shared_map_ops.map_ops

(** NOTE bt_find and bt_handle_detach are named for the particular
   application we envisage: a persistent cache which hands over to a
   btree *)
(** 

Construct the PCACHE, which uses the pcache_ops and wraps it in a routine which occasionally executes a B-tree roll-up. 

Parameters:

- [monad_ops]

- [pcache_ops]: pcache from tjr_pcache, with pcache interface

- [pcache_blocks_limit]: how many blocks in the pcache before
  attempting a roll-up; if the length of pcache is [>=] this limit, we
  attempt a roll-up; NOTE that this limit should be >= 2 (if we roll
  up with 1 block, then in fact nothing gets rolled up because we roll
  up "upto" the current block; not a problem but probably pointless
  for testing)

- [bt_find]: called if key not in pcache map FIXME do we need a
  write-through cache here? or just rely on the front-end LRU? FIXME
  note that even if a rollup is taking place, we can use the old
  B-tree root for the [bt_find] operation.

- [bt_handle_detach]: called to detach the rollup into another thread;
  typically this operation puts a msg on a message queue which is then
  received and acted upon by the dedicated rollup thread

  *)
let make_ops
      ~monad_ops 
      ~(pcache_ops:('k,'v,'ptr,'kvop_map,'t) pcache_ops)
      ~pcache_blocks_limit 
      ~bt_find
      ~(bt_handle_detach:('k,'v,'ptr,'kvop_map) detach_info -> (unit,'t)m)
  =
  (* let open Mref_plus in *)
  let ( >>= ) = monad_ops.bind in
  let return = monad_ops.return in
  let pc = pcache_ops in  (* persistent cache; another name for pcache *)
  let find k = 
    pc.find k >>= fun v ->
    match v with
    | None -> bt_find k
    | Some v -> return (Some v)
  in
  let maybe_roll_up () = 
    pc.blk_len () >>= fun n ->
    match n >= pcache_blocks_limit with
    | false -> return `No_roll_up_needed
    | true -> 
      (* Printf.printf "pcache_thread, maybe_roll_up\n%!"; *)
      pc.detach () >>= fun detach_result ->
      bt_handle_detach detach_result >>= fun () ->
      return `Ok
  in
  let insert k v =
    pc.insert k v >>= fun () -> 
    maybe_roll_up () >>= fun _ ->
    return ()
  in
  let delete k =
    pc.delete k >>= fun () -> 
    maybe_roll_up () >>= fun _ ->
    return ()
  in
  let insert_many k v kvs = 
    (* FIXME we should do something smarter here *)
    insert k v >>= fun () -> return kvs
  in
  Tjr_fs_shared.Shared_map_ops.{find;insert;delete;insert_many}

let _ = make_ops
