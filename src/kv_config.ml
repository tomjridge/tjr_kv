module Config_type = struct
  type config = {
    lru_max_size                 :int;
    lru_evict_count              :int;
    dmap_ops_per_block           :int;
    dmap_blocks_limit            :int;
    test_thread_yield_iterations :int;  (* num iterations between yields *)
    test_thread_delay_iterations :int;  (* num iterations before sleeping *)
    test_thread_delay            :float;
    dmap_thread_delay            :float;
    bt_thread_delay              :float;
    bt_filename                  :string;
    dmap_filename                :string; (* where to store the dmap *)
  } [@@deriving yojson]
end
include Config_type

module S = struct
  include Config_type
  let default_config = Some {
      lru_max_size                 =256;
      lru_evict_count              =128;
      dmap_ops_per_block           =200;
      dmap_blocks_limit            =10;
      test_thread_yield_iterations =50;
      test_thread_delay_iterations =100;
      test_thread_delay            =(1e-6);
      dmap_thread_delay            =(1e-6);
      bt_thread_delay              =(1e-6);
      bt_filename                  ="btree.store";
      dmap_filename                ="dmap.store"
    }
  let filename="tjr_kv_config.json"
end

include Tjr_config.Make(S)

let { lru_max_size; lru_evict_count; dmap_ops_per_block;
      dmap_blocks_limit; dmap_thread_delay; bt_thread_delay; bt_filename; dmap_filename; _ } =
  config

[%%import "kv_optcomp_config.ml"]
[%%if PROFILING_ENABLED]
let profiling_enabled = true
[%%else]
let profiling_enabled = false
[%%endif]
