let now = Core.Time_stamp_counter.(fun () ->
    now () |> to_int63 |> Core.Int63.to_int |> fun (Some x) -> x)[@ocaml.warning "-8"]


let profile_function name f = 
  let x = now () in
  let r = f () in
  let y = now () in
  Printf.printf "%s finished, total time: %d\n%!" name (y-x);
  r



(* let kvop_map_ops = Op_aux.default_kvop_map_ops ()  *)
(* let empty_map = kvop_map_ops.empty *)
