(*

(** Functional store, with a reference for initialization purposes *)

(* test store -------------------------------------------------------- *)

(* FIXME this is a common pattern when initializing a store; perhaps
   add to Tjr_store *)
let set,get = Tjr_store.(set,get)

let test_store = ref (Tjr_store.empty_fstore ())

let mk_ref' v = 
  !test_store |> fun t ->
  Tjr_store.mk_ref v t |> fun (r,t) ->
  test_store:=t;
  r

*)
