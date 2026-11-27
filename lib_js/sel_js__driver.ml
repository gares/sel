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


(* The driver of Sel_js.run. It is not in Sel_js since the backend wakes it
   up, and Sel_js depends on Sel_core, which depends on the backend: for the same
   reason [run] takes [Sel_core.pop_ready] and [Sel_core.Todo.add] as arguments *)

open Js_of_ocaml

let global name = Js.Unsafe.get Js.Unsafe.global (Js.string name)

let now () =
  Js.float_of_number (Js.Unsafe.meth_call (global "Date") "now" [||]) /. 1000.

let set_timeout f seconds =
  let ms = Js.number_of_float (seconds *. 1000.) in
  Js.Unsafe.fun_call (global "setTimeout")
    [| Js.Unsafe.inject (Js.wrap_callback f); Js.Unsafe.inject ms |]

let clear_timeout id =
  ignore (Js.Unsafe.fun_call (global "clearTimeout") [| Js.Unsafe.inject id |])

(* Set by [run] *)
let wakeup_driver : (unit -> unit) option ref = ref None

let wakeup () = Option.iter (fun f -> f ()) !wakeup_driver

let run ~pop_ready ~add ?(budget=0.02) ?(poll=0.05) ~handle_event ~init () =
  if Option.is_some !wakeup_driver then failwith "Sel_js.run: called twice";
  (* The todo of the driver: the events returned by [handle_event] are added
     to it, and it is not visible outside of the driver *)
  let todo = ref init in
  (* Handle ready events for [budget] seconds at most, [true] if more events
     may be ready *)
  let step () =
    let t0 = now () in
    let rec loop () =
      match pop_ready !todo with
      | None, t -> todo := t; false
      | Some ev, t ->
          todo := t;
          todo := add !todo (handle_event ev);
          if now () -. t0 > budget then true else loop ()
    in
    loop () in
  (* At most one run of [go] is pending: its delay and its timeout.
     Scheduling with a shorter delay replaces it (e.g. [wakeup] while
     sleeping for [poll] seconds). *)
  let pending = ref None in
  let rec schedule delay =
    match !pending with
    | Some (d, _) when d <= delay -> ()
    | Some (_, id) ->
        clear_timeout id;
        pending := Some (delay, set_timeout go delay)
    | None ->
        pending := Some (delay, set_timeout go delay)
  and go () =
    pending := None;
    let more = try step () with e -> schedule 0.; raise e in
    if more then schedule 0.
    else if poll < infinity then schedule poll
  in
  wakeup_driver := Some (fun () -> schedule 0.);
  schedule 0.
