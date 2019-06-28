module S = struct
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
    } [@@deriving yojson]
  end
  include Config_type
  let default_config = Some {
      lru_max_size                 =256;
      lru_evict_count              =128;
      dmap_ops_per_block           =50;
      dmap_blocks_limit            =10;
      test_thread_yield_iterations =50;
      test_thread_delay_iterations =100;
      test_thread_delay            =(1e-6);
      dmap_thread_delay            =(1e-6);
      bt_thread_delay              =(1e-6)
    }
  let filename="tjr_kv_config.json"
end
include S.Config_type

include Tjr_config.Make(S)
