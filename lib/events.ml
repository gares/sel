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

type 'a res = ('a,exn) result

(* Events in progress *)
type _ in_progress =
  | Line : Bytes.t * Buffer.t -> string in_progress
  | Bytes : int * Bytes.t -> Bytes.t in_progress

(* Reified function composition *)
type _ fcomp =
  | FNil : 'a -> 'a fcomp
  | FCons : 'a in_progress * ('a res -> 'b fcomp) -> 'b fcomp

let rec map_fcomp f = function
  | FNil x -> FNil (f x)
  | FCons(pending,g) -> FCons(pending,(fun x -> map_fcomp f (g x)))

let finish_with (k : 'a res -> 'b) x = FNil(k x)
let (--) = fun x xs -> FCons(x,xs)
(* locally one does let (--?) x y = err k x y in  as a cons with error handling *)
let err : type a b c. (c res -> b) -> a in_progress -> (a -> b fcomp) -> b fcomp =
  fun k x xs ->
    let xs = function
      | Ok v -> xs v
      | Error e -> FNil(k (Error e)) in
    FCons(x,xs)

(* Read events can be composed of many read operations *)
type 'a system_event =
  | ReadInProgress of Unix.file_descr * 'a fcomp
  | WaitProcess of int * (Unix.process_status -> 'a)
let pp_system_event _ fmt = function
  | ReadInProgress(_,_) -> Stdlib.Format.fprintf fmt "ReadInProgress"
  | WaitProcess(pid,_) -> Stdlib.Format.fprintf fmt "WaitProcess %d" pid

type 'a queue_event =
  | WaitQueue1 : 'b Queue.t * ('b -> 'a) -> 'a queue_event
  | WaitQueue2 : 'b Queue.t * 'c Queue.t * ('b -> 'c -> 'a) -> 'a queue_event

let pp_queue_event _ fmt = function
  | WaitQueue1 _ -> Stdlib.Format.fprintf fmt "WaitQueue1"
  | WaitQueue2 _ -> Stdlib.Format.fprintf fmt "WaitQueue2"

type 'a task_event = 'a [@@deriving show]

module Event = struct

type cancellation_handle = bool ref [@@deriving show]

module WithAttributes = struct
type 'a t = {
  name : string option ;
  priority : Sorted.priority;
  it : 'a;
  cancelled : cancellation_handle;
}
[@@deriving show]

let map f { name; priority; cancelled; it } =
  let e = { name; priority; cancelled; it = f it } in
  e

let on_it f { it; _ } = f it

end



let make_event ?(priority=0) ?name it =
  let cancelled = ref false in
  let e = { WithAttributes.name; priority = { Sorted.default_priority with user = priority }; cancelled; it } in
  e

type 'a event_ =
  | SystemEvent of 'a system_event
  | QueueEvent of 'a queue_event
  | Task of 'a task_event
  [@@deriving show]

let map_system_event f = function
  | WaitProcess(pid,k) -> WaitProcess(pid, (fun x -> f (k x)))
  | ReadInProgress(fd,fcomp) -> ReadInProgress(fd,map_fcomp f fcomp)
  
let map_queue_event f = function
  | WaitQueue1(q1,k) -> WaitQueue1(q1,(fun x -> f (k x)))
  | WaitQueue2(q1,q2,k) -> WaitQueue2(q1,q2,(fun x y -> f (k x y)))

let map_task_event f x = f x

let map f = function
  | Task e -> Task (map_task_event f e)
  | SystemEvent e -> SystemEvent (map_system_event f e)
  | QueueEvent e -> QueueEvent(map_queue_event f e)  
  
type 'a t = 'a event_ WithAttributes.t
[@@deriving show]
  
let map f e = WithAttributes.map (map f) e

let get_cancellation_handle { WithAttributes.cancelled; _ } = cancelled
let cancel x = x := true

end

open Event

type ('a,'b) has_finished =
  | Yes of 'a
  | No  of 'b

let mkReadInProgress fd = function
  | FCons _ as f -> No (ReadInProgress(fd,f))
  | FNil x -> Yes x

let one_line () = Line (Bytes.make 1 '0', Buffer.create 40)
let some_bytes n ?(buff=Bytes.create n) () = Bytes(n,buff)

module On = struct

  type 'a res = ('a,exn) result

let line ?priority ?name fd k : 'a Event.t =
  make_event ?priority ?name @@ SystemEvent (ReadInProgress(fd, one_line () -- finish_with k))

let bytes ?priority ?name fd n k : 'a Event.t =
  make_event ?priority ?name @@ SystemEvent (ReadInProgress(fd, some_bytes n () -- finish_with k))

let death_of ?priority ?name ~pid k : 'a Event.t =
  make_event ?priority ?name @@ SystemEvent (WaitProcess(pid,k))

let an_ocaml_value (k : 'a res -> 'b) : 'b fcomp =
  let (--?) x y = err k x y in
  some_bytes Marshal.header_size ()
  --? (fun buff -> let data_size = Marshal.data_size buff 0 in
  some_bytes data_size ~buff:(Bytes.extend buff 0 data_size) ()
  --? (fun buff -> let value = Marshal.from_bytes buff 0 in
  finish_with k (Ok value)))
;;

let ocaml_value ?priority ?name fd k : 'a Event.t =
  make_event ?priority ?name @@ SystemEvent (ReadInProgress(fd, an_ocaml_value k))

let parse_content_length_or err k s =
  try
    let input = Scanf.Scanning.from_string (String.uppercase_ascii s) in
    Scanf.bscanf input "CONTENT-LENGTH: %d" k
  with
    (Scanf.Scan_failure _ | Failure _ | End_of_file | Invalid_argument _) as e ->
      err (Error e)

let an_httpcle (k : Bytes.t res -> 'b) : 'b fcomp  =
  let (--?) x y = err k x y in
  one_line ()
  --? (parse_content_length_or (finish_with k) (fun length ->
  one_line ()
  --? (fun _discard ->
  some_bytes length ()
  --? (fun buff ->
  finish_with k (Ok buff)))))

let httpcle ?priority ?name fd k : 'a Event.t =
  make_event ?priority ?name @@ SystemEvent (ReadInProgress(fd, an_httpcle k))

let queue ?priority ?name q1 k : 'a Event.t =
  make_event ?priority ?name @@ QueueEvent (WaitQueue1(q1,k))

let queues ?priority ?name q1 q2 k : 'a Event.t =
  make_event?priority ?name  @@ QueueEvent (WaitQueue2(q1,q2,k))

end

let now ?priority ?name task : 'a Event.t =
  make_event ?priority ?name @@ Task task
  
let advance_queue _ = function
  | WaitQueue1(q1,_) as x when Queue.is_empty q1 ->  No x
  | WaitQueue1(q1,k) -> Yes(k (Queue.pop q1))
  | WaitQueue2(q1,q2,_) as x when Queue.is_empty q1 || Queue.is_empty q2 -> No x
  | WaitQueue2(q1,q2,k) -> Yes(k (Queue.pop q1) (Queue.pop q2))

let advance_system ~ready_fds _ = function
  | WaitProcess(pid,k) as x ->
      let rc, code = Unix.waitpid [Unix.WNOHANG] pid in
      if rc == 0 then No x
      else Yes (k code)
  | ReadInProgress(_, FNil _) -> assert false
  | ReadInProgress(fd,_) as x when not (List.mem fd ready_fds) -> No x
  | ReadInProgress(fd, FCons(Line (buff,acc) as line,rest)) -> begin try
      let n = Unix.read fd buff 0 1 in
      if n = 0 then begin
        Buffer.clear acc;
        mkReadInProgress fd (rest (Error End_of_file))
      end else
        let c = Bytes.get buff 0 in
        if c != '\n' then begin
          Buffer.add_char acc c; 
          mkReadInProgress fd (FCons(line,rest))
        end else begin
          let one_line = Buffer.contents acc in
          Buffer.clear acc;
          mkReadInProgress fd (rest (Ok one_line))
        end
      with Unix.Unix_error _ as e ->
        Buffer.clear acc;
        mkReadInProgress fd (rest (Error e))
      end
  | ReadInProgress(fd,FCons(Bytes(n,buff),rest)) -> begin try
      let m = Unix.read fd buff (Bytes.length buff - n) n in
      if m = 0 then
        mkReadInProgress fd (rest (Error End_of_file))
      else
        if m != n then
          mkReadInProgress fd (FCons(Bytes(n-m,buff),rest))
        else
          mkReadInProgress fd (rest (Ok buff))
      with Unix.Unix_error _ as e -> mkReadInProgress fd (rest (Error e))
      end
      