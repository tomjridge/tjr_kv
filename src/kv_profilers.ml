[%%import "kv_optcomp_config.ml"]

module Internal : sig
  val lru_profiler : int profiler
  val dmap_profiler: int profiler
  val bt_profiler : int profiler
end = struct
  [%%if PROFILING_ENABLED]
  let lru_profiler = make_profiler()
  let dmap_profiler = make_profiler()
  let bt_profiler = make_profiler()
  [%%else]
  let d = dummy_profiler
  let lru_profiler = d
  let dmap_profiler = d
  let bt_profiler = d
  [%%endif]
end

include Internal

