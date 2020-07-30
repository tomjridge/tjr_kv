open Shared_ctxt

type 'r kv_origin = { pcache_origin: 'r Pl_origin.t; btree_root: 'r }[@@deriving bin_io]

module Origin = struct
  type t = Shared_ctxt.r kv_origin[@@deriving bin_io]
  let max_sz = 9*2
end

let bp_mshlr : _ bp_mshlr = (module Origin)

let ba_mshlr = bp_mshlrs#ba_mshlr ~mshlr:bp_mshlr ~buf_sz:(Blk_sz.to_int blk_sz)

module M = (val ba_mshlr)

(* FIXME this should probably have origin parameterized on blk_id,
   like everything else; but at the moment we have only 1 use case *)
type ('blk_id,'blk,'t) root_manager = <
  with_: 
    blk_dev_ops:('blk_id,'blk,'t) blk_dev_ops -> 
    <
      read_origin: 'blk_id -> (Origin.t,'t)m;
      write_origin: blk_id:'blk_id -> origin:Origin.t -> (unit,'t)m;
    >
>

(** Example root managers *)
let root_managers = 
  let for_lwt_ba_buf : ('blk_id,_,_) root_manager = 
    let with_ ~blk_dev_ops = 
      let read_origin blk_id = 
        blk_dev_ops.read ~blk_id >>= fun blk -> 
        M.unmarshal blk |> return
      in
      let write_origin ~blk_id ~origin = 
        origin |> M.marshal |> fun blk -> 
        blk_dev_ops.write ~blk_id ~blk
      in
      object
        method read_origin=read_origin
        method write_origin=write_origin
      end
    in
    object
      method with_=with_
    end
  in
  object
    method for_lwt_ba_buf=for_lwt_ba_buf
  end


let _ = root_managers
