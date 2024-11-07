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

(************************ UTILS **********************************************)

(* we don't want to lock forever doing tests, esp if we know pop_opt would be
   stuck *)
let wait_timeout todo =
  let ready, todo = pop_timeout ~stop_after_being_idle_for:0.1 todo in
  [%test_eq: bool] (Option.is_none ready) true;
  [%test_eq: bool] (Todo.is_empty todo) false;
  ready, todo

(* match a string list against a rex list, useful for errors *)
let osmatch r s =
  match s with
  | None -> false
  | Some s -> Str.string_match (Str.regexp r) s 0
  
let b2s = function
  | Ok b -> Bytes.to_string b
  | Error x -> Stdlib.Printexc.to_string x

let s2s = function
  | Ok s -> s
  | Error x -> Stdlib.Printexc.to_string x

let write_pipe write s =
  let len = String.length s in
  let rc = Unix.write write (Bytes.of_string s) 0 len in
  [%test_eq: int] rc len

let pipe () =
  let read, write = Unix.pipe () in
  read, write_pipe write

let read_leftover read n =
  let b = Bytes.create n in
  let rc = Unix.read read b 0 n in
  [%test_eq: int] rc n;
  Bytes.to_string b
  
(*****************************************************************************)

let %test_unit "sel.event.http_cle" =
  let read, write = pipe () in
  let e = On.httpcle read b2s in
  let t0 = Unix.gettimeofday () in
  let n = 99999 in
  for _i = 1 to n do
    let todo = Todo.add Todo.empty [e] in
    write "content-Length: 4\n\n1\n3.";
    let ready, todo = pop_opt todo in
    [%test_eq: bool] (Todo.is_empty todo) true;
    [%test_eq: string option] ready (Some "1\n3.");
  done;
  let t1 = Unix.gettimeofday () in
  Stdlib.Printf.eprintf "time to pop %d httpcle events: %f\n" n (t1 -. t0)
;;
