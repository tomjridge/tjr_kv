module Config_type = struct
  type config = {
    tst_thrd_yld_its    : int; (** iters between yields *)

    tst_thrd_dly_its    : int; (** iters before sleeping *)

    tst_thrd_dly        : float; (** secs to delay, proportionality cst *)
    lru_max_size        : int;
    lru_evict_count     : int;
    filename            : string; (** store filename *)
    pcache_blocks_limit : int;  (** max blocks before detaching *)
  } [@@deriving yojson]
end
include Config_type

module S = struct
  include Config_type
  let default_config = Some {
      tst_thrd_yld_its     = 1000;
      tst_thrd_dly_its     = 10000;
      tst_thrd_dly         = (1e-5);
      lru_max_size         = 256;
      lru_evict_count      = 128;
      filename             = "kv.store";
      pcache_blocks_limit  = 10;
    }
  let filename="kv_config.json"
end

include Tjr_config.Make(S)

[%%import "kv_conf_optcomp.ml"]
[%%if PROFILING_ENABLED]
let _ : unit = Printf.printf "%s: profiling enabled\n" __FILE__
let profiling_enabled = true
[%%else]
let profiling_enabled = false
[%%endif]
