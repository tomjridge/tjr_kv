[%%import "kv_config_optcomp.ml"]

module Internal : sig
  val lru_profiler : int profiler
  val pcache_profiler: int profiler
  val bt_profiler : int profiler
end = struct
  [%%if PROFILING_ENABLED]
  let lru_profiler = make_profiler ~print_header:"kv lru profiler" ()
  let pcache_profiler = make_profiler ~print_header:"kv pcache profiler" ()
  let bt_profiler = make_profiler ~print_header:"kv bt profiler" ()
  [%%else]
  let d = dummy_profiler
  let lru_profiler = d
  let pcache_profiler = d
  let bt_profiler = d
  [%%endif]
end

include Internal

