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

let write_pipe write s =
  let len = String.length s in
  let rc = Unix.write write (Bytes.of_string s) 0 len in
  [%test_eq: int] rc len

let pipe () =
  let read, write = Unix.pipe () in
  read, write_pipe write

(*****************************************************************************)

(* pop_opt terminates *)
let%test_unit "sel.loop" =
  let read, write = pipe () in
  let e = On.httpcle ~priority:1 read (fun x -> Result.map ~f:Bytes.to_string x) in
  write "Content-Length: 3\n\n123\n";
  let x = Sel.now ~priority:2 (Ok "bad") in
  let todo = Todo.add Todo.empty [e;x] in
  let loop todo =
    let ready, _todo = Sel.pop todo in
    match ready with
    | Ok "bad" -> [%test_eq: string] "" "bad1"
    | Ok "123" -> ()
    | Ok _ -> [%test_eq: string] "" "bad2"
    | Error e -> [%test_eq: string] "" (Stdlib.Printexc.to_string e) in
  loop todo
  
let%test_unit "sel.loop2" =
  let read, write = pipe () in
  let e = On.httpcle ~priority:1 read (fun x -> Result.map ~f:Bytes.to_string x) in
  write "Content-Length: 3\n\n12";
  let x = Sel.now ~priority:2 (Ok "bad") in
  let todo = Todo.add Todo.empty [e;x] in
  let loop todo =
    let ready, _todo = Sel.pop todo in
    match ready with
    | Ok "bad" -> ()
    | Ok _ -> [%test_eq: string] "" "bad2"
    | Error e -> [%test_eq: string] "" (Stdlib.Printexc.to_string e) in
  loop todo
  
(* pop_opt terminates *)
let%test_unit "sel.loop3" =
  let read, write = pipe () in
  let e = On.line ~priority:1 read (fun x -> x) in
  write "aa\nbb\ncc\n";
  let read2, write2 = pipe () in
  let x = On.bytes ~priority:2 read2 2 (function Error e -> Error e | Ok s -> Error (Failure (Stdlib.Format.asprintf "lower priority event triggered: '%s'" (Bytes.to_string s)))) in
  let todo = Todo.add Todo.empty [e;x] in
  let rec loop todo =
    let ready, todo = Sel.pop todo in
    match ready with
    | Ok "cc" -> ()
    | Ok s -> write2 s; loop (Todo.add todo [e])
    | Error End_of_file -> ()
    | Error e -> [%test_eq: string] "" (Stdlib.Printexc.to_string e) in
  loop todo
  
