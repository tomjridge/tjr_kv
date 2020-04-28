(** {2 Manage a set of object roots; don't open} *)

open Kv_intf

(** Construct as a tuple *)
let make_1 
    ~monad_ops 
    ~(blk_ops:'blk blk_ops) 
    ~(blk_dev_ops:('blk_id,'blk,'t)blk_dev_ops) 
    ~blk_id 
  = 
  let module Blk_id = Blk_id_as_int in
  let ( >>= ) = monad_ops.bind in
  let return = monad_ops.return in
  let {read;write;_} = blk_dev_ops in
  (* let blk_0 = Blk_id_as_int.of_int 0 in *)
  let sync_blk blk_id = return () in (* FIXME *)
  let read_roots () = 
    read ~blk_id >>= fun blk ->
    Marshal.from_string (blk_ops.to_string blk) 0 |> fun (a:'roots) -> 
    return a
  in
  let write_roots ?(sync=true) a =
    Marshal.to_string a [] |> fun blk_as_string -> 
    Test.assert_(fun () -> 
        assert(String.length blk_as_string <= (blk_ops.blk_sz|>Blk_sz.to_int)));
    blk_as_string |> blk_ops.of_string |> fun blk -> 
    write ~blk_id ~blk >>= fun () ->
    sync_blk blk_id
  in
  (read_roots,write_roots)

open Std_types

let make_3 ~blk_dev_ops ~blk_id : (_,_) root_man = 
  let (read_roots,write_roots) = make_1 ~monad_ops ~blk_ops ~blk_dev_ops ~blk_id in
  object 
    method read_roots=read_roots
    method write_roots=write_roots ~sync:true
  end

let _ = make_3


