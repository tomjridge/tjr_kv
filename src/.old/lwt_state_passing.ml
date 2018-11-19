(** We use Lwt to handle concurrency; in addition, our model requires
   state passing (where the state type is from {! Fun_store}). So we
   need to construct an instance of the relevant monad.

In order to interact nicely with Lwt, we have to ensure that only one
   promise "owns" the state at any point. But this is hard to
   reconcile with multiple promises executing simultaneously.

Instead, we have used [with_xxx] type functions to identify particular
   parts of the global state that need to be locked while a promise
   executes. This enables much finer-grained concurrency. In fact,
   most concurrent accesses to the same state occur when placing
   messages on msg queues. (There is also [with_lru], for obtaining
   the lru state.)

Then the question becomes how to model all of this in a faithful
   way. One option is to have a "store" thread, which is responsible
   for locking and releasing parts of the state, and also reclaiming
   and unlocking at the end of the [with_xxx] functions. (Queues can
   be handled locally, using the queue lock and cvar of course.)

Thus, [with_lru] might take the global store lock, then the lru_state
   lock, then return the lru_state to the continuation; when the lwt
   continuation resolves, we can obtain the global store lock, and
   unlock the lru_state lock.

The final question is how to model the global store as an Lwt process,
   in a functional way. But this can be done simply by building on
   basic Lwt primitives, and then exposing these to other threads.

  *)


(* example ---------------------------------------------------------- *)
open Lwt

type t = {
  global_lock : Lwt_mutex.t;
  part1: int;
  part1_lock: Lwt_mutex.t;
  part2: int;
  part2_lock: Lwt_mutex.t;
}

let make_state_thread ~async =
  let mb = Lwt_mvar.create_empty () in
  let rec loop state = 
    Lwt_mvar.take mb >>= fun msg ->
    match msg with
    | `With_part1 kk -> (
        let set_part1 x = failwith "" in
        Lwt_mutex.lock state.global_lock >>= fun () ->
        Lwt_mutex.lock state.part1_lock >>= fun () ->
        async(kk ~state:state.part1 ~set_part1 >>= fun a ->
              (Lwt_mutex.unlock state.part1_lock; return ()) >>= fun () ->
              return a);
        (Lwt_mutex.unlock state.global_lock; loop state))
    | `With_part2 kk -> (
        let set_part2 x = failwith "" in
        Lwt_mutex.lock state.global_lock >>= fun () ->
        Lwt_mutex.lock state.part2_lock >>= fun () ->
        async(kk ~state:state.part2 ~set_part2 >>= fun a ->
              (Lwt_mutex.unlock state.part1_lock; return ()) >>= fun () ->
              return a);
        (Lwt_mutex.unlock state.global_lock; loop state))
  in
  fun state -> loop state
      
(* one problem may be that we have to explicitly define all these
   components, but with the funstore that may be fairly
   straightforward *)




