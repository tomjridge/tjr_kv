(* FIXME remove the other With_lwt module that is clashing with Tjr_monad.With_lwt *)

let _ = 
  let module Test = Kv_store_with_lru.Test() in
  Lwt_main.run (Tjr_monad.With_lwt.to_lwt @@ Test.test())

(*
module Requires = struct
  module Bt_blk_id = Tjr_int.Make_type_isomorphic_to_int()
  module Pc_blk_id = Tjr_int.Make_type_isomorphic_to_int()
end


module Ukv = Tjr_kv.Synchronous_store.Make(Requires)

module Test = Ss_test.Test()

let main () = Test.run_tests ~depth:4

let _ = main()
*)
