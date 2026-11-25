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

(** Simple event library, Unix part

    The [sel.unix] library implements {!Sel} with a backend based on
    [Unix.select] (it is the default implementation), and this module
    complements {!Sel} with events on Unix file descriptors and processes.

    The main loop goes like this

    {[

    type top_event =
      | NotForMe of Component.Event.t
      | Echo of string

    let echo : top_event Sel.Event.t =
      Sel_unix.On.line Unix.stdin (function
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

(** Unix events one can wait for (read data, death of a process, ...)
    - [name] for debug printing
    - [priority] lower integers correspond to high priorities (as in Unix nice),
      default is 0
*)
module On : sig

  (** a line, terminated by ['\n'] *)
  val line : ?priority:int -> ?name:string ->
    Unix.file_descr -> (string Sel.On.res -> 'a) -> 'a Sel.Event.t

  (** bytes *)
  val bytes :  ?priority:int -> ?name:string ->
    Unix.file_descr -> int -> (Bytes.t Sel.On.res -> 'a) -> 'a Sel.Event.t

  (** termination of a process *)
  val death_of : ?priority:int -> ?name:string ->
    pid:int -> (Unix.process_status -> 'a) -> 'a Sel.Event.t

  (** any value (not type safe, uses [Marshall]) *)
  val ocaml_value : ?priority:int -> ?name:string ->
    Unix.file_descr -> ('b Sel.On.res -> 'a) -> 'a Sel.Event.t

  (** HTTP Content Length encoded data *)
  val httpcle : ?priority:int -> ?name:string ->
    Unix.file_descr -> (Bytes.t Sel.On.res -> 'a) -> 'a Sel.Event.t

end
