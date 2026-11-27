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


(** Simple event library, JavaScript part

    The [sel.js] library implements {!Sel_core} for programs compiled with
    js_of_ocaml or wasm_of_ocaml, running in a browser (or a Web Worker) or
    in node, and this module provides a driver for the main loop.

    The blocking APIs ({!Sel_core.pop}, {!Sel_core.wait}, ...) fail when no
    event is ready, and the program should let {!val:run} handle the events instead.
    The backend uses [Date.now] as a clock and calls {!val:wakeup} when a
    promise is resolved.

    {[

    let inbox : string Queue.t = Queue.create ()

    let handle_event = function
      | Echo text -> print_endline text; [echo]
      ...

    let () =
      Sel_js.run ~handle_event
        ~init:(Sel_core.Todo.add Sel_core.Todo.empty [echo; ...]) ();
      (* JavaScript side calls this function when a message arrives *)
      let on_message s = Queue.push s inbox; Sel_js.wakeup () in
      ...

    ]}

 *)

(** [run ~handle_event ~init ()] starts the driver, which handles the ready
    events with [handle_event]. The driver has its own todo, initially
    [init]: the events returned by [handle_event] are added to it, and they
    are the only way to add events to it.

    [run] returns immediately: events are handled by functions scheduled
    with [setTimeout], each one handling ready events, one after the other,
    and giving control back to the JavaScript event loop when no event is
    ready or when more than [budget] seconds (default [0.02]) have passed.
    The time is checked between two events, [handle_event] is not
    interrupted: a long computation should be split in steps with
    {!val:Sel_core.now}.

    When no event is ready the driver sleeps until {!val:wakeup} is called
    or [poll] seconds (default [0.05]) have passed. The poll is needed by
    events which are not woken up by {!val:wakeup}, e.g. system events
    without a file descriptor. [poll] can be [infinity].

    There is only one driver, and it runs forever: calling [run] twice
    fails.
    If [handle_event] raises an exception the event is lost, the driver
    keeps running and the exception is raised to the JavaScript event loop.
*)
val run : ?budget:float -> ?poll:float ->
  handle_event:('a -> 'a Sel_core.Event.t list) -> init:'a Sel_core.Todo.t -> unit -> unit

(** [wakeup ()] makes the driver handle the ready events as soon as
    possible. It must be called when data is made available to an event
    from outside of the driver, e.g. when a JavaScript callback pushes a
    message in a queue waited by {!val:Sel_core.On.queue}. *)
val wakeup : unit -> unit
