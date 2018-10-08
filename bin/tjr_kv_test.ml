
module Requires = struct
  module Bt_blk_id = Tjr_int.Make_type_isomorphic_to_int()
  module Pc_blk_id = Tjr_int.Make_type_isomorphic_to_int()
end


module Ukv = Tjr_kv.Uncached_persistent_map_impl.Make(Requires)

module Test = Ukv.Test()

let main () = Test.run_tests ~depth:4

let _ = main()
