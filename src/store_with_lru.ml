(** A KV store with the LRU frontend. *)

(** We construct:

- Lru
- q_lru_dcl (msg queue from lru to dcl)
- DCL (detachable chunked list)
- DCL thread (Dcl_t)
- q_dcl_btree (msg queue from dcl to btree)
- B-tree
- B-tree thread (Btree_t)

We connect:

- Lru -> Dcl_t with a message queue
- Dcl_t -> Btree_t with a message queue

*)

(* monad ops -------------------------------------------------------- *)

(* use lwt *)

open Tjr_monad.Lwt_monad_instance

let monad_ops = lwt_ops

let event_ops = 

(* lru -------------------------------------------------------------- *)

open Tjr_lru_cache.Lru_multithreaded


