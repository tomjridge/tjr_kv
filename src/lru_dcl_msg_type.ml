open Tjr_monad.Types
open Tjr_lru_cache.Entry

type ('k,'v,'t) lru_dcl_msg 
  = ('k,'v,'t) Tjr_lru_cache.Msg_type.msg
  =  Insert of 'k*'v*(unit -> (unit,'t)m)
  | Delete of 'k*(unit -> (unit,'t)m)
  | Find of 'k * ('v option -> (unit,'t)m)
  | Evictees of ('k * 'v entry) list



