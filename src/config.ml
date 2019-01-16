module S = struct
  module Config_type = struct
    type config = {
      lru_max_size:int;
      lru_evict_count:int;
      dmap_ops_per_block:int;
      dmap_blocks_limit:int;
      test_thread_delay:float;
      dmap_thread_delay:float;
      bt_thread_delay:float;
    } [@@deriving yojson]
  end
  include Config_type
  let filename="config.json"
end
include S.Config_type

include Tjr_config.Make(S)
