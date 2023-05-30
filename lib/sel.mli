(**************************************************************************)
(*                                                                        *)
(*                                 SEL                                    *)
(*                                                                        *)
(*                   Copyright INRIA and contributors                     *)
(*       (see version control and README file for authors & dates)        *)
(*                                                                        *)
(**************************************************************************)
(*                                                                        *)
(*   This file is distributed under the terms of the MIT License.         *)
(*   See LICENSE file.                                                    *)
(*                                                                        *)
(**************************************************************************)

(** Simple event library - No async monads, No threads, No exceptions *)

type 'a event
type 'a res = ('a,exn) result
type cancellation_handle
val cancel : cancellation_handle -> unit

(** System events one can wait for *)
val on_line : Unix.file_descr -> (string res -> 'a) -> 'a event * cancellation_handle
val on_bytes : Unix.file_descr -> int -> (Bytes.t res -> 'a) -> 'a event * cancellation_handle
val on_death_of : pid:int -> (Unix.process_status -> 'a) -> 'a event * cancellation_handle

val on_ocaml_value : Unix.file_descr -> ('b res -> 'a) -> 'a event * cancellation_handle
val on_httpcle : Unix.file_descr -> (Bytes.t res -> 'a) -> 'a event * cancellation_handle

(** Synchronization events between components (worker pool and a task queue) *)
val on_queues : 'b Queue.t -> 'c Queue.t -> ('b -> 'c -> 'a) -> 'a event * cancellation_handle

(** A way to feed the event queue *)
val on_queue : 'b Queue.t -> ('b -> 'a) -> 'a event * cancellation_handle

(** Mix regular computations with blocking event (reified) *)
val now : 'a -> 'a event

val map : ('a -> 'b) -> 'a event -> 'b event

(** for debug printing *)
val name : string -> 'a event -> 'a event
(** a recurrent event is never removed from the todo set, that is, when
   ready a copy of it is added back automatically *)
val make_recurring : 'a event -> 'a event
(** convenience adaptor to drop the cancellation handle *)
val uncancellable : 'a event * cancellation_handle -> 'a event
(** it is unusual to make a regular computation cancellable, hence the
   event constructor does not return the cancellation handle. Here the way
   to recover it *)
val cancellation_handle : 'e event -> cancellation_handle
(** lower integers correspond to high priorities (as in Unix nice) *)
val set_priority : int -> 'a event -> 'a event

(** The main loop goes like this

    type top_event =
    | NotForMe of Component.event
    | Echo of string

    let echo : top_event event =
      on_line Unix.stdin (function
        | Ok s -> Echo s
        | Error _ -> Echo "error")
      |> uncancellable
      |> make_recurrent

    let handle_event = function
    | NotForMe e ->
        List.map (map (fun x -> NotForMe x)) (Component.handle_event e)
    | Echo text ->
        Printf.eprintf "echo: %s\n" text;
        []

    let rec loop evs =
      let ready, evs = pop evs in
      let new_evs = handle_event ready in
      loop (enqueue evs new_evs)

    let main () =
      loop (enqueue empty [echo; ...])

 *)

(** The set of events we can wait for *)
type 'a todo

(** The empty todo set *)
val empty : 'a todo

(** Check if the todo set is empty *)
val is_empty : 'a todo -> bool

(** In presence of recurring events the todo set is never empty *)
val only_recurring_events : 'a todo -> bool

(* Debugging *)
val pp_todo : (Format.formatter -> 'a -> unit) ->
  Format.formatter -> 'a todo -> unit
val size : 'a todo -> int

(** Adds a list of events, the order (among events with the same priority)
   is preserved *)
val enqueue : 'a todo -> 'a event list -> 'a todo

(** Wait for one event. If more are ready, return the one with higher priority.
    raises Failure if there is nothing left to do *)
val pop : 'a todo -> 'a * 'a todo

(** Same as [pop] but retuning an option *)
val pop_opt : 'a todo -> 'a option * 'a todo

(** Same as [pop_opt] but retuning an [None] if no event is ready
    in [stop_after_being_idle_for] seconds. Precision is about a tenth of
    a second. *)
val pop_timeout : stop_after_being_idle_for:float ->
  'a todo -> 'a option * 'a todo

(** Waits until some event is ready. The three lists are, respectively
    system events, synchronization events, and other events.
    All system and synchronization events which are ready are returned,
    and are sorted according to the priority (higher priority first).
    A computation is considered only if it has higher priority than any
    other event which is ready.
    
    This API cannot be mixed with [pop], use one or the other.
    *) 
val wait : 'a todo -> 'a list * 'a list * 'a option * 'a todo

(* Same as [wait] but returns empty lists if no event is ready
    in [stop_after_being_idle_for] seconds. Precision is about a tenth of
    a second. *)
val wait_timeout : stop_after_being_idle_for:float ->
  'a todo -> 'a list * 'a list * 'a option * 'a todo
