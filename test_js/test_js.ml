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


(* Run with node. The driver never polls: the test terminates only if the
   driver is woken up by Sel_js.wakeup and by the resolution of a promise *)

open Js_of_ocaml

let set_timeout ms f =
  ignore (Js.Unsafe.fun_call (Js.Unsafe.get Js.Unsafe.global (Js.string "setTimeout"))
    [| Js.Unsafe.inject (Js.wrap_callback f); Js.Unsafe.inject (Js.number_of_float ms) |])

type ev = Msg of string | Resolved of int Sel_core.Promise.state | Step of int

let inbox : string Queue.t = Queue.create ()
let msg = Sel_core.On.queue inbox (fun s -> Msg s)

let p, r = Sel_core.Promise.make ()

let now () =
  Js.float_of_number (Js.Unsafe.meth_call (Js.Unsafe.get Js.Unsafe.global (Js.string "Date")) "now" [||]) /. 1000.

let t0 = now ()

let handle_event = function
  | Msg s -> print_endline ("msg " ^ s); [msg]
  | Step n when n > 0 -> print_endline ("step " ^ string_of_int n); [Sel_core.now (Step (n-1))]
  | Step _ -> print_endline "steps done"; []
  | Resolved (Sel_core.Promise.Fulfilled n) ->
      print_endline ("resolved " ^ string_of_int n);
      let t1 = now () in
      (* the clock is in seconds and we waited at least 20ms *)
      assert (t1 -. t0 >= 0.015 && t1 -. t0 < 5.);
      []
  | Resolved (Sel_core.Promise.Rejected _) -> assert false

let () =
  Sel_js.run ~poll:infinity ~handle_event
    ~init:(Sel_core.Todo.add Sel_core.Todo.empty [msg; Sel_core.now (Step 2); Sel_core.On.promise p (fun x -> Resolved x)]) ();
  print_endline "run returned";
  (match Sel_js.run ~handle_event ~init:Sel_core.Todo.empty () with
   | exception Failure _ -> print_endline "run twice fails"
   | () -> assert false);
  (* one cannot block *)
  (match Sel_core.pop (Sel_core.Todo.add Sel_core.Todo.empty [Sel_core.On.queue (Queue.create ()) Fun.id]) with
   | exception Failure _ -> print_endline "pop fails"
   | _ -> assert false);
  set_timeout 10. (fun () -> Queue.push "hello" inbox; Sel_js.wakeup ());
  set_timeout 20. (fun () -> Sel_core.Promise.fulfill r 42)
