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

(** Simple event library

    The [sel] library implements {!Sel_core} with a backend based on
    [Unix.select] (it is the default implementation), and this module
    extends {!Sel_core} with events on Unix file descriptors and processes
    (see {!On}).

    Libraries which only create events (queues, promises, {!Sel_core.now})
    and that should also work in JavaScript can depend on [sel.core] and use
    {!Sel_core} instead: the types of the two modules are the same.

    The main loop goes like this

    {[

    type top_event =
      | NotForMe of Component.Event.t
      | Echo of string

    let echo : top_event Sel.Event.t =
      Sel.On.line Unix.stdin (function
        | Ok s -> Echo s
        | Error _ -> Echo "error")

    let handle_event = function
      | NotForMe e ->
          Component.handle_event e |>
          List.map (Sel.Event.map (fun x -> NotForMe x))
      | Echo text ->
          Printf.eprintf "echo: %s\n" text;
          [echo]

    let rec loop evs =
      let ready, evs = Sel.pop evs in
      let new_evs = handle_event ready in
      loop (Sel.Todo.add evs new_evs)

    let main () =
      loop (Sel.Todo.add Sel.Todo.empty [echo; ...])

    ]}

 *)

include module type of struct include Sel_core end
  with module On := Sel_core.On

(** Events one can wait for (read data, pull from queues, death of a
    process, ...)
    - [name] for debug printing
    - [priority] lower integers correspond to high priorities (as in Unix nice),
      default is 0
*)
module On : sig

  include module type of struct include Sel_core.On end

  (** a line, terminated by ['\n'] *)
  val line : ?priority:int -> ?name:string ->
    Unix.file_descr -> (string res -> 'a) -> 'a Event.t

  (** bytes *)
  val bytes :  ?priority:int -> ?name:string ->
    Unix.file_descr -> int -> (Bytes.t res -> 'a) -> 'a Event.t

  (** termination of a process *)
  val death_of : ?priority:int -> ?name:string ->
    pid:int -> (Unix.process_status -> 'a) -> 'a Event.t

  (** any value (not type safe, uses [Marshall]) *)
  val ocaml_value : ?priority:int -> ?name:string ->
    Unix.file_descr -> ('b res -> 'a) -> 'a Event.t

  (** HTTP Content Length encoded data *)
  val httpcle : ?priority:int -> ?name:string ->
    Unix.file_descr -> (Bytes.t res -> 'a) -> 'a Event.t

end
