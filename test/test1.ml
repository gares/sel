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
let%test_unit "sel.wait.empty" =
  let ready, todo = pop_opt Todo.empty in
  [%test_eq: bool] (Option.is_none ready) true;
  [%test_eq: bool] (Todo.is_empty todo) true;
;;

(* tasks are returned according to their priority *)
let %test_unit "sel.wait.prio" =
  let todo = Todo.add Todo.empty [
    now ~priority:3 3;
    now ~priority:1 1;
    now ~priority:2 2
    ] in
  let ready, todo = pop_opt todo in
  [%test_eq: int option] ready (Some 1);
  let ready, todo = pop_opt todo in
  [%test_eq: int option] ready (Some 2);
  let ready, _todo = pop_opt todo in
  [%test_eq: int option] ready (Some 3);
;;

(* ready list not empty with pop *)
let %test_unit "sel.wait.pop" =
  let e1 = now 1 in
  let q = Stdlib.Queue.create () in
  let e2 = On.queue q (fun x -> x) in
  let e3 = On.queue q (fun x -> x) in
  Stdlib.Queue.push 2 q;
  Stdlib.Queue.push 3 q;
  let todo = Todo.add Todo.empty [e1;e2;e3] in
  let x, todo = pop_opt todo in
  let y, todo = pop_opt todo in
  let z, todo = pop_opt todo in
  [%test_eq: bool] (Todo.is_empty todo) true;
  [%test_eq: int option] x (Some 1);
  [%test_eq: int option] y (Some 2);
  [%test_eq: int option] z (Some 3);
;;


(* bytes n waits until n bytes are read *)
let %test_unit "sel.event.bytes" =
  let read, write = pipe () in
  let todo = Todo.add Todo.empty [On.bytes read 3 b2s] in
  (* nothing to read *)
  let _ready, todo = wait_timeout todo in
  (* something to read but not enough *)
  write "1";
  let _ready, todo = wait_timeout todo in
  (* more than enough *)
  write "234";
  let ready, todo = pop_opt todo in
  [%test_eq: bool] (Todo.is_empty todo) true;
  [%test_eq: string option] ready (Some "123");
  (* extra byte is not lost *)
  [%test_eq: string] (read_leftover read 1) "4";
;;

(* internal buffer is reset *)
let %test_unit "sel.event.bytes.recurring" =
  let read, write = pipe () in
  let e = On.bytes read 3 b2s in
  let todo = Todo.add Todo.empty [e] in
  write "123";
let ready, todo = pop_opt todo in
  [%test_eq: string option] ready (Some "123");
  let todo = Todo.add todo [e] in
  write "456";
  let ready, _todo = pop_opt todo in
  [%test_eq: string option] ready (Some "456");
;;

(* queue does not run unless something is pushed *)
let%test_unit "sel.event.queue" =
  let q = Stdlib.Queue.create () in
  let todo = Todo.add Todo.empty [On.queue q (fun () -> ())] in
  (* no progress since the queue is empty *)
  let _ready, todo = wait_timeout todo in
  (* progress since the queue has a token *)
  Stdlib.Queue.push () q;
  let ready, todo = pop_opt todo in
  [%test_eq: bool] (Option.is_none ready) false;
  [%test_eq: bool] (Todo.is_empty todo) true;
;;

(* queue does not run unless something is pushed *)
let%test_unit "sel.event.queue" =
  let q = Stdlib.Queue.create () in
  let todo = Todo.add Todo.empty [On.queue_all q (fun () l -> l)] in
  (* no progress since the queue is empty *)
  let _ready, todo = wait_timeout todo in
  (* progress since the queue has a token *)
  Stdlib.Queue.push () q;
  Stdlib.Queue.push () q;
  Stdlib.Queue.push () q;
  let ready, todo = pop_opt todo in
  [%test_eq: bool] (Option.is_none ready) false;
  [%test_eq: unit list option] ready (Some [();()]);
  [%test_eq: bool] (Todo.is_empty todo) true;
  [%test_eq: bool] (Stdlib.Queue.is_empty q) true;
;;

(* queue2 does not advance unless both queues are pushed *)
let%test_unit "sel.event.queue2" =
  let q1 = Stdlib.Queue.create () in
  let q2 = Stdlib.Queue.create () in
  let todo = Todo.add Todo.empty [On.queues q1 q2 (fun () () -> ())] in
  Stdlib.Queue.push () q1;
  (* no progress since one queue is empty *)
  let _ready, todo = wait_timeout todo in
  Stdlib.Queue.push () q2;
  (* progress since both queues have a token *)
  let ready, todo = pop_opt todo in
  [%test_eq: bool] (Option.is_none ready) false;
  [%test_eq: bool] (Todo.is_empty todo) true;
;;

(* Promise is like q singleton queue *)
let%test_unit "sel.event.promise" =
  let p, r = Promise.make () in
  let todo = Todo.add Todo.empty [On.promise p (fun e -> e)] in
  (* no progress since one fulfilled *)
  let _ready, todo = wait_timeout todo in
  Promise.fulfill r 7;
  let ready, todo = pop_opt todo in
  [%test_eq: bool] (Option.is_none ready) false;
  let n =
    match ready with
    | None -> assert false
    | Some (Promise.Fulfilled n) -> n
    | Some _ -> Int.max_value in
  [%test_eq: int ] n 7;
  [%test_eq: bool] (Todo.is_empty todo) true;
;;

(* line event waits for \n and does not eat more chars *)
let %test_unit "sel.event.line" =
  let read, write = pipe () in
  let todo = Todo.add Todo.empty [On.line read s2s] in
  let _ready, todo = wait_timeout todo in
  write "123";
  let _ready, todo = wait_timeout todo in
  write "\naa";
  let ready, todo = pop_opt todo in
  [%test_eq: bool] (Todo.is_empty todo) true;
  [%test_eq: string option] ready (Some "123");
  [%test_eq: string] (read_leftover read 2) "aa";
;;

(* line internal buffer is not pulluted by previous run *)
let %test_unit "sel.event.line.recurring" =
  let read, write = pipe () in
  let e = On.line read s2s in
  let todo = Todo.add Todo.empty [e] in
  write "123\n";
  let ready, todo = pop_opt todo in
  [%test_eq: bool] (Todo.is_empty todo) true;
  [%test_eq: string option] ready (Some "123");
  let todo = Todo.add todo [e] in
  write "456\n";
  let ready, todo = pop_opt todo in
  [%test_eq: bool] (Todo.is_empty todo) true;
  [%test_eq: string option] ready (Some "456");
;;

let %test_unit "sel.event.http_cle" =
  let read, write = pipe () in
  let todo = Todo.add Todo.empty [On.httpcle read b2s ] in
  write "content-Length: 4\n\n1\n3.";
  let ready, todo = pop_opt todo in
  [%test_eq: bool] (Todo.is_empty todo) true;
  [%test_eq: string option] ready (Some "1\n3.");
;;

let %test_unit "sel.event.http_cle.recurring" =
  let read, write = pipe () in
  let e = On.httpcle read b2s in
  let todo = Todo.add Todo.empty [e] in
  write "content-Length: 4\n\n1\n3.";
  let ready, todo = pop_opt todo in
  [%test_eq: bool] (Todo.is_empty todo) true;
  [%test_eq: string option] ready (Some "1\n3.");
  let todo = Todo.add todo [e] in
  write "content-Length: 4\n\n4\n6.";
  let ready, todo = pop_opt todo in
  [%test_eq: bool] (Todo.is_empty todo) true;
  [%test_eq: string option] ready (Some "4\n6.");
;;

let %test_unit "sel.event.http_cle.recurring.err" =
  let read, write = pipe () in
  let e = On.httpcle read b2s in
  let todo = Todo.add Todo.empty [e] in
  write "content-Length: \n";
  let ready, todo = pop_opt todo in
  [%test_eq: bool] (Todo.is_empty todo) true;
  [%test_eq: string option] ready (Some "End_of_file");
  let todo = Todo.add todo [e] in
  write "a\n";
  let ready, todo = pop_opt todo in
  [%test_eq: bool] (Todo.is_empty todo) true;
  (* depending on how eager the event is to read data *)
  [%test_eq: bool] (osmatch "\\(.*Scan_failure.*\\|End_of_file\\)" ready) true;
  let todo = Todo.add todo [e] in
  write "content-Lengtx: 2\n";
  let ready, todo = pop_opt todo in
  [%test_eq: bool] (Todo.is_empty todo) true;
  [%test_eq: bool] (osmatch ".*Scan_failure.*" ready) true;
  let todo = Todo.add todo [e] in
  write "content-Length: 4\n\n4\n6.";
  let ready, todo = pop_opt todo in
  [%test_eq: bool] (Todo.is_empty todo) true;
  [%test_eq: string option] ready (Some "4\n6.");
;;

let %test_unit "sel.event.now.nodup" =
  let e1 = now ~priority:1 ~undup:(=) 1 in
  let e2 = now ~priority:1 1 in
  let todo = Todo.add Todo.empty [e1;e2] in
  let ready, todo = wait todo in
  [%test_eq: bool] (Todo.is_empty todo) true;
  [%test_eq: int list] ready [1];
;;  

let %test_unit "sel.event.now.order0" =
  let e1 = now ~priority:1 1 in
  let e2 = now ~priority:1 2 in
  let e3 = now ~priority:2 3 in
  let e4 = now ~priority:2 4 in
  let q = Stdlib.Queue.create () in
  Stdlib.Queue.add 0 q;
  let x = On.queue ~priority:1 q (fun x -> x) in
  let todo = Todo.add Todo.empty [x;e1;e3] in
  let todo = Todo.add todo [e2;e4] in
  let ready, todo = wait todo in
  [%test_eq: int list] ready [0;1;2];
  let ready, todo = wait todo in
  [%test_eq: int list] ready [3;4];
  [%test_eq: bool] (Todo.is_empty todo) true;
  let ready, _ = wait todo in
  [%test_eq: int list] ready [];
;;  

let %test_unit "sel.event.now.order1" =
  let e1 = now ~priority:1 1 in
  let e2 = now ~priority:1 2 in
  let e3 = now ~priority:2 3 in
  let e4 = now ~priority:2 4 in
  let q = Stdlib.Queue.create () in
  Stdlib.Queue.add 0 q;
  let x = On.queue ~priority:1 q (fun x -> x) in
  let todo = Todo.add Todo.empty [e1;x;e3] in
  let todo = Todo.add todo [e2;e4] in
  let ready, todo = wait todo in
  [%test_eq: int list] ready [1;0;2];
  let ready, todo = wait todo in
  [%test_eq: int list] ready [3;4];
  [%test_eq: bool] (Todo.is_empty todo) true;
  let ready, _ = wait todo in
  [%test_eq: int list] ready [];
;;  
  
