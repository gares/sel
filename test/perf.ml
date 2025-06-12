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

let %test_unit "sel.event.http_cle.perf" =
  Caml.Gc.compact ();
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
let%test_unit "sel.event.promise.perf" =
  Caml.Gc.compact ();
  let many = 30_000 in (* Loc of Pfff.v *)
  let x0 = Unix.gettimeofday () in
  for i = 1 to many do
    let p, r = Promise.make () in
    let todo = Todo.add Todo.empty [On.promise p (fun e -> e)] in
    (* no progress since one fulfilled *)
    Promise.fulfill r i;
    let ready, todo = pop todo in
    let n =
      match ready with
      | (Promise.Fulfilled n) -> n
      | _ -> Int.max_value in
    [%test_eq: int ] n i;
    [%test_eq: bool] (Todo.is_empty todo) true;
  done;
  let x1 = Unix.gettimeofday () in
  Stdlib.Printf.eprintf "resolving %d promises: %4.3f\n" many (x1 -. x0);
;;
let%test_unit "sel.event.promise.thread.spawn.perf" =
  Caml.Gc.compact ();
  let many = 30_000 in (* Loc of Pfff.v *)
  let x0 = Unix.gettimeofday () in
  for i = 1 to many do
    let p, r = Promise.make () in
    let todo = Todo.add Todo.empty [On.promise p (fun e -> e)] in
    (* no progress since one fulfilled *)
    let t = Thread.create (fun () -> Promise.fulfill r i) () in
    let ready, todo = pop todo in
    Thread.join t;
    let n =
      match ready with
      | (Promise.Fulfilled n) -> n
      | _ -> Int.max_value in
    [%test_eq: int ] n i;
    [%test_eq: bool] (Todo.is_empty todo) true;
  done;
  let x1 = Unix.gettimeofday () in
  Stdlib.Printf.eprintf "resolving %d promises via thread spawning: %4.3f\n" many (x1 -. x0);
;;
let%test_unit "sel.event.promise.thread.synchronization.perf" =
  Caml.Gc.compact ();
  let many = 30_000 in (* Loc of Pfff.v *)

  let c = Condition.create () in
  let m = Mutex.create () in
  let x = ref None in
  let t = Thread.create (fun () ->
    let stop = ref false in
    while not !stop do
      Mutex.lock m;
      while match !x with None -> true | _ -> false do Condition.wait c m; done;
      match !x with
      | None -> assert false
      | Some (p,i) -> Promise.fulfill p i; x := None; Condition.signal c; Mutex.unlock m; stop := i = many
    done) () in

  let x0 = Unix.gettimeofday () in
  for i = 1 to many do
    let p, r = Promise.make () in
    let todo = Todo.add Todo.empty [On.promise p (fun e -> e)] in

    Mutex.lock m;
    while match !x with None -> false | _ -> true do Condition.wait c m; done;
    x := Some (r,i);
    Condition.signal c; Mutex.unlock m;

    let ready, todo = pop todo in
    let n =
      match ready with
      | (Promise.Fulfilled n) -> n
      | _ -> Int.max_value in
    [%test_eq: int ] n i;
    [%test_eq: bool] (Todo.is_empty todo) true;
  done;
  let x1 = Unix.gettimeofday () in
  Thread.join t;
  Stdlib.Printf.eprintf "resolving %d promises via thread synchronization: %4.3f\n" many (x1 -. x0);
;;
