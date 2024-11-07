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
  
let b2s = function
  | Ok b -> Bytes.to_string b
  | Error x -> Stdlib.Printexc.to_string x

let write_pipe write s =
  let len = String.length s in
  let rc = Unix.write write (Bytes.of_string s) 0 len in
  [%test_eq: int] rc len

let pipe () =
  let read, write = Unix.pipe () in
  read, write_pipe write
  
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
