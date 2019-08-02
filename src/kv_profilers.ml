[%%import "kv_optcomp_config.ml"]

[%%if PROFILING_ENABLED]
module Lru_profiler = Tjr_profile.With_array.Make_profiler(struct let cap = int_of_float 1e7 end)
module Dmap_profiler = Tjr_profile.With_array.Make_profiler(struct let cap = int_of_float 1e7 end)
module Bt_profiler = Tjr_profile.With_array.Make_profiler(struct let cap = int_of_float 1e7 end)
[%%else]
module Lru_profiler = Tjr_profile.Dummy_int_profiler
module Dmap_profiler = Lru_profiler
module Bt_profiler = Lru_profiler
[%%endif]

