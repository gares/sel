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

(* pop_opt terminates *)
let%test_unit "sel.loop" =
  let read, write = pipe () in
  let e = On.httpcle ~priority:1 read (fun x -> Result.map ~f:Bytes.to_string x) in
  write "Content-Length: 3\n\n123\n";
  let x = Sel.now ~priority:2 (Ok "bad") in
  let todo = Todo.add Todo.empty [e;x] in
  let rec loop todo =
    let ready, _todo = Sel.pop todo in
    match ready with
    | Ok "bad" -> [%test_eq: string] "" "bad1"
    | Ok "123" -> ()
    | Ok _ -> [%test_eq: string] "" "bad2"
    | Error e -> [%test_eq: string] "" (Stdlib.Printexc.to_string e) in
  loop todo
  
