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

(** Simple event library *)

(** The main loop goes like this 

    {[

    type top_event =
      | NotForMe of Component.Event.t
      | Echo of string

    let echo : top_event Event.t =
      On.line Unix.stdin (function
        | Ok s -> Echo s
        | Error _ -> Echo "error")
      |> Event.recurring

    let handle_event = function
      | NotForMe e ->
          Component.handle_event e |>
          List.map (Event.map (fun x -> NotForMe x))
      | Echo text ->
          Printf.eprintf "echo: %s\n" text;
          []

    let rec loop evs =
      let ready, evs = pop evs in
      let new_evs = handle_event ready in
      loop (Todo.add evs new_evs)

    let main () =
      loop (Todo.add Todo.empty [echo; ...])

    ]}

 *)

 
 (** The type of events and operations (setting priority, cancelling, ...)*)
 module Event : sig

  type 'a t

  (** pretty printer *)
  val pp : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit

  (** inject an event into another one *)
  val map : ('a -> 'b) -> 'a t -> 'b t

  (** for debug printing *)
  val name : string -> 'a t -> 'a t

  (** a recurrent event is never removed from the todo set, that is, when
      ready a copy of it is added back automatically *)
  val recurring : 'a t -> 'a t

  (** lower integers correspond to high priorities (as in Unix nice) *)
  val at_priority : int -> 'a t -> 'a t

  type cancellation_handle

  (** in order to cancel an event, one has to store its cancellation handle *)
  val get_cancellation_handle : 'e t -> cancellation_handle

  (** a cancelled event is automatically removed from the todo set *)
  val cancel : cancellation_handle -> unit

end

(** Events one can wait for (read data, pull from queues, ...) *)
module On : sig

  type 'a res = ('a,exn) result

  (** a line, terminated by ['\n'] *)
  val line : Unix.file_descr -> (string res -> 'a) -> 'a Event.t

  (** bytes *)
  val bytes : Unix.file_descr -> int -> (Bytes.t res -> 'a) -> 'a Event.t

  (** termination of a process *)
  val death_of : pid:int -> (Unix.process_status -> 'a) -> 'a Event.t

  (** any value (not type safe, uses [Marshall]) *)
  val ocaml_value : Unix.file_descr -> ('b res -> 'a) -> 'a Event.t

  (** HTTP Content Length encoded data *)
  val httpcle : Unix.file_descr -> (Bytes.t res -> 'a) -> 'a Event.t

  (** Synchronization events between two components (e.g. a worker pool and a
      task queue) and an event (e.g. starting a worker) *)
  val queues : 'b Queue.t -> 'c Queue.t -> ('b -> 'c -> 'a) -> 'a Event.t

  (** Synchronization events between a component and an event *)
  val queue : 'b Queue.t -> ('b -> 'a) -> 'a Event.t

end

(** mix a regular computations with blocking events. E.g.
    to make a [fold] interruptible one can do something like:

    {[
    type event =
      | Fold of (int -> int -> int) * int * int list
      | Other

    let handle_event = function
      | Other -> ...
      | Fold(_,acc,[]) -> Printf "done: %s\n" acc; []
      | Fold(f,acc,x::xs) -> [now (Fold (f, f acc x, xs))

    let main =
      let rec loop todo =
        match pop_opt todo with
        | None -> exit 0
        | Some e ->
            let es = handle_event e in
            loop (Todo.add todo es)
      in
        loop (Todo.add Todo.empty [now (Fold((+),0,[1;2;3])); ...])
    ]}

*)
val now : 'a -> 'a Event.t

(** Set of events being waited for *)
module Todo : sig

  (** The set of events we can wait for *)
  type 'a t

  (** the empty todo set *)
  val empty : 'a t

  (** check if the todo set is empty *)
  val is_empty : 'a t -> bool

  (** in presence of recurring events the todo set is never empty *)
  val only_recurring_events : 'a t -> bool

  (** pretty printer *)
  val pp : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit

  (** debugging *)
  val size : 'a t -> int

  (** adds a list of events, the order (among events with the same priority)
    is preserved *)
  val add : 'a t -> 'a Event.t list -> 'a t

end

(** wait for one event. If more are ready, return the one with higher priority.
    @raise Failure when there is nothing left to do *)
val pop : 'a Todo.t -> 'a * 'a Todo.t

(** same as {!val:pop} but retuning an option *)
val pop_opt : 'a Todo.t -> 'a option * 'a Todo.t

(** same as {!val:pop_opt} but retuning a [None] if no event is ready
    in [stop_after_being_idle_for] seconds. Precision is about a tenth of
    a second. *)
val pop_timeout : stop_after_being_idle_for:float ->
  'a Todo.t -> 'a option * 'a Todo.t

(** waits until some event is ready. The three lists are, respectively
    system events, synchronization events (see {!val:On.queue} and {!val:On.queues}),
    and other events (see {!val:now}).
    All system and synchronization events which are ready are returned,
    and are sorted according to the priority (higher priority first).
    A computation is considered only if it has higher priority than any
    other event which is ready.
    
    This API cannot be mixed with {!val:pop}, use one or the other.
    *) 
val wait : 'a Todo.t -> 'a list * 'a list * 'a option * 'a Todo.t

(** Same as {!val:wait} but returns empty lists if no event is ready
    in [stop_after_being_idle_for] seconds. Precision is about a tenth of
    a second. *)
val wait_timeout : stop_after_being_idle_for:float ->
  'a Todo.t -> 'a list * 'a list * 'a option * 'a Todo.t
