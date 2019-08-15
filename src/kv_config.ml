module Config_type = struct
  type config = {
    test_thread_yield_iterations :int;  (* num iterations between yields *)
    test_thread_delay_iterations :int;  (* num iterations before sleeping *)
    test_thread_delay            :float;

    lru_max_size                 :int;
    lru_evict_count              :int;

    dmap_filename                :string; (* where to store the dmap *)
    dmap_ops_per_block           :int;
    dmap_blocks_limit            :int;
    dmap_thread_delay            :float;

    bt_filename                  :string;
    bt_thread_delay              :float;
 
    root_man_filename            :string;
  } [@@deriving yojson]
end
include Config_type

module S = struct
  include Config_type
  let default_config = Some {
      test_thread_yield_iterations =50;
      test_thread_delay_iterations =100;
      test_thread_delay            =(1e-6);

      lru_max_size                 =256;
      lru_evict_count              =128;

      dmap_filename                ="dmap.store";
      dmap_ops_per_block           =200;
      dmap_blocks_limit            =10;
      dmap_thread_delay            =(1e-6);

      bt_filename                  ="btree.store";
      bt_thread_delay              =(1e-6);

      root_man_filename            ="root_man.store";
    }
  let filename="tjr_kv_config.json"
end

include Tjr_config.Make(S)

let { 
  test_thread_yield_iterations;
  test_thread_delay_iterations;
  test_thread_delay;
  
  lru_max_size; 
  lru_evict_count; 
  
  dmap_filename;
  dmap_ops_per_block;
  dmap_blocks_limit; 
  dmap_thread_delay; 

  bt_filename;
  bt_thread_delay;

  root_man_filename
} = config

[%%import "kv_optcomp_config.ml"]
[%%if PROFILING_ENABLED]
let profiling_enabled = true
[%%else]
let profiling_enabled = false
[%%endif]
