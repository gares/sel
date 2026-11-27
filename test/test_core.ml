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
open Base
open Sel_core

(* drain all ready events, never blocking *)
let rec drain acc todo =
  match pop_ready todo with
  | None, todo -> List.rev acc, todo
  | Some x, todo -> drain (x :: acc) todo

let%test_unit "sel.core.pop_ready.empty" =
  let q = Stdlib.Queue.create () in
  let todo = Todo.add Todo.empty [On.queue q (fun x -> x)] in
  let t0 = Stdlib.Sys.time () in
  let l, todo = drain [] todo in
  [%test_eq: string list] l [];
  [%test_eq: bool] (Todo.is_empty todo) false;
  [%test_eq: bool] Float.(Stdlib.Sys.time () -. t0 < 0.05) true

let%test_unit "sel.core.pop_ready.external_loop" =
  let q = Stdlib.Queue.create () in
  let p, r = Promise.make () in
  let todo = Todo.add Todo.empty [
    On.queue q (fun s -> "msg:" ^ s);
    On.promise p (function Promise.Fulfilled n -> "promise:" ^ Int.to_string n | Rejected _ -> "rejected");
    now ~priority:(-1) "task" ] in
  let l, todo = drain [] todo in
  [%test_eq: string list] l ["task"];
  (* an external event loop fills the queue and resolves the promise *)
  Stdlib.Queue.push "hello" q;
  Promise.fulfill r 3;
  let l, todo = drain [] todo in
  [%test_eq: string list] l ["msg:hello"; "promise:3"];
  [%test_eq: bool] (Todo.is_empty todo) true

let%test_unit "sel.core.wait_ready" =
  let q = Stdlib.Queue.create () in
  let todo = Todo.add Todo.empty [On.queue q (fun x -> x)] in
  let l, todo = wait_ready todo in
  [%test_eq: string list] l [];
  Stdlib.Queue.push "x" q;
  let l, todo = wait_ready todo in
  [%test_eq: string list] l ["x"];
  [%test_eq: bool] (Todo.is_empty todo) true
