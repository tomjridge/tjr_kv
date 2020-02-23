(** {2 Manage a set of object roots} *)

open Kv_intf
open Root_man_ops

let make_root_man ~monad_ops ~(blk_ops:'blk blk_ops) ~(blk_dev_ops:('blk_id,'blk,'t)blk_dev_ops) = 
  let module Blk_id = Blk_id_as_int in
  let ( >>= ) = monad_ops.bind in
  let return = monad_ops.return in
  let {read;write;_} = blk_dev_ops in
  let blk_0 = Blk_id_as_int.of_int 0 in
  let sync_blk blk_id = return () in (* FIXME *)
  let read_roots () = 
    read ~blk_id:blk_0 >>= fun blk ->
    Marshal.from_string (blk_ops.to_string blk) 0 |> fun (a:'roots) -> 
    return a
  in
  let write_roots ?(sync=true) a =
    Marshal.to_string a [] |> fun blk_as_string -> 
    Test.assert_(fun () -> assert(String.length blk_as_string <= (blk_ops.blk_sz|>Blk_sz.to_int)));
    blk_as_string |> blk_ops.of_string |> fun blk -> 
    write ~blk_id:blk_0 ~blk >>= fun () ->
    sync_blk blk_0
  in
  { read_roots; write_roots }

let _ :
monad_ops:'t monad_ops ->
blk_ops:'blk blk_ops ->
blk_dev_ops:(Blk_id_as_int.blk_id, 'blk, 't) blk_dev_ops ->
('roots, 't) root_man
= make_root_man
