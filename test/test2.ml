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
open Sel

let wait_timeout todo =
  let ready, todo = pop_timeout ~stop_after_being_idle_for:0.1 todo in
  [%test_eq: bool] (Option.is_none ready) true;
  [%test_eq: bool] (Todo.is_empty todo) false;
  ready, todo

(* Thread synchronization *)
let%test_unit "sel.event.promise.thread1" =
  let p, r = Promise.make () in
  let todo = Todo.add Todo.empty [On.promise p (fun e -> e)] in
  (* no progress since one fulfilled *)
  let _ready, todo = wait_timeout todo in
  let t = Thread.create (fun () -> Thread.delay 1.0; Promise.fulfill r 7) () in
  let ready, todo = pop_opt todo in
  Thread.join t;
  [%test_eq: bool] (Option.is_none ready) false;
  let n =
    match ready with
    | None -> assert false
    | Some (Promise.Fulfilled n) -> n
    | Some _ -> Int.max_value in
  [%test_eq: int ] n 7;
  [%test_eq: bool] (Todo.is_empty todo) true;
;;
let%test_unit "sel.event.promise.thread2" =
  let p, r = Promise.make () in
  let todo = Todo.add Todo.empty [On.promise p (fun e -> e)] in
  (* no progress since one fulfilled *)
  let _ready, todo = wait_timeout todo in
  let t = Thread.create (fun () -> Promise.fulfill r 7) () in
  let ready, todo = pop_opt todo in
  Thread.join t;
  [%test_eq: bool] (Option.is_none ready) false;
  let n =
    match ready with
    | None -> assert false
    | Some (Promise.Fulfilled n) -> n
    | Some _ -> Int.max_value in
  [%test_eq: int ] n 7;
  [%test_eq: bool] (Todo.is_empty todo) true;
;;