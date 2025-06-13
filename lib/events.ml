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
  | Line : int * Bytes.t * Buffer.t -> string in_progress
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

module Promise = struct

  type 'a state =
    | Fulfilled of 'a
    | Rejected of exn [@printer fun fmt e -> Format.fprintf fmt "%s" (Printexc.to_string e)]
  [@@deriving show]

  type file_descr = Unix.file_descr
  let pp_file_descr =
    if Sys.unix then fun fmt i -> Format.fprintf fmt "<fd:%d>" (Obj.magic i)
    else fun fmt _ -> Format.fprintf fmt "<fd:?>"

  type 'a t = 'a state option ref * file_descr
  [@@deriving show]

  type 'a handler = 'a t

  let resolved_msg : Bytes.t = Bytes.create 1

  let resolve (p,w) s =
    if !p = None then (p := Some s; ignore @@ Unix.write w resolved_msg 0 1 ; Unix.close w)
    else raise @@ Failure "cannot resolve a promise twice"

  let make () : 'a t * 'a handler =
    let p = ref None in
    let r, w = Unix.pipe () in
    (p, r), (p, w)
  
  let is_resolved (p,_) = !p <> None
  let get (p,_) =
    match !p with
    | Some x -> x
    | None -> raise @@ Failure "promise not resolved"

  let fulfill r x = resolve r (Fulfilled x)

  let reject r x = resolve r (Rejected x)
end

type 'a queue_event =
  | WaitQueue1 : 'b Queue.t * ('b -> 'a) -> 'a queue_event
  | WaitQueueBatch1 : 'b Queue.t * ('b -> 'b list -> 'a) -> 'a queue_event
  | WaitQueue2 : 'b Queue.t * 'c Queue.t * ('b -> 'c -> 'a) -> 'a queue_event

let pp_queue_event _ fmt = function
  | WaitQueue1 _ -> Stdlib.Format.fprintf fmt "WaitQueue1"
  | WaitQueueBatch1 _ -> Stdlib.Format.fprintf fmt "WaitQueueBatch1"
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
  | WaitQueueBatch1(q1,k) -> WaitQueueBatch1(q1,(fun x xs -> f (k x xs)))
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
  | Advanced of 'b

let mkReadInProgress fd = function
  | FCons _ as f -> Advanced (ReadInProgress(fd,f))
  | FNil x -> Yes x

let one_line ?(at_least=1) () = Line (at_least,Bytes.make at_least '0', Buffer.create (max 40 at_least))
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
      
let len_httpcle_header = String.length "CONTENT-LENGTH: \n"

let an_httpcle (k : Bytes.t res -> 'b) : 'b fcomp  =
  let (--?) x y = err k x y in
  one_line ~at_least:len_httpcle_header ()
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

let queue_all ?priority ?name q1 k : 'a Event.t =
  make_event ?priority ?name @@ QueueEvent (WaitQueueBatch1(q1,k))
  
let queues ?priority ?name q1 q2 k : 'a Event.t =
  make_event?priority ?name  @@ QueueEvent (WaitQueue2(q1,q2,k))

let a_promise_resolution (_,r as p) (k : 'a Promise.state -> 'b) : 'b fcomp =
  let k_res = function
    | Ok _ -> Unix.close r;
        (* if there was no error, the code of Promise.resolve did its job before writing *)
        assert(Promise.is_resolved p);
        k (Promise.get p)
    | Error exn -> Unix.close r;
        (* Maybe we got signalled, ... or maybe this id dead, hence correct, code  *)
        if Promise.is_resolved p then k (Promise.get p)
        else k (Promise.Rejected exn) in
  let (--?) x y = err k_res x y in
  some_bytes 1 ~buff:Promise.resolved_msg () (* same buffer we use to read *)
  --? (fun buff ->
  finish_with k_res (Ok buff))

let promise ?priority ?name (_,r as p : 'a Promise.t) (k : 'a Promise.state -> 'b) : 'b Event.t =
  make_event?priority ?name @@ SystemEvent (ReadInProgress(r,a_promise_resolution p k))

end

let now ?priority ?name task : 'a Event.t =
  make_event ?priority ?name @@ Task task
  
let advance_queue _ _ = function
  | WaitQueue1(q1,_) as x when Queue.is_empty q1 -> (), No x
  | WaitQueue1(q1,k) -> (), Yes(k (Queue.pop q1))
  | WaitQueueBatch1(q1,_) as x when Queue.is_empty q1 -> (), No x
  | WaitQueueBatch1(q1,k) ->
      let hd = Queue.pop q1 in
      let tl = ref [] in
      while not @@ Queue.is_empty q1 do
        tl := Queue.pop q1 :: !tl
      done;
       (), Yes(k hd (List.rev !tl))
  | WaitQueue2(q1,q2,_) as x when Queue.is_empty q1 || Queue.is_empty q2 -> (), No x
  | WaitQueue2(q1,q2,k) -> (), Yes(k (Queue.pop q1) (Queue.pop q2))

let advance_system ready_fds _ = function
  | WaitProcess(pid,k) as x ->
      let rc, code = Unix.waitpid [Unix.WNOHANG] pid in
      if rc == 0 then ready_fds, No x
      else ready_fds, Yes (k code)
  | ReadInProgress(_, FNil _) -> assert false
  | ReadInProgress(fd,_) as x when not (List.mem fd ready_fds) -> ready_fds, No x
  | ReadInProgress(fd, FCons(Line (m,buff,acc),rest)) ->
    let ready_fds = List.filter ((<>) fd) ready_fds in
    ready_fds,
    begin try
      let n = Unix.read fd buff 0 m in
      if n = 0 then begin
        Buffer.clear acc;
        mkReadInProgress fd (rest (Error End_of_file))
      end else
        let c = Bytes.get buff (n-1) in
        if c != '\n' then begin
          Buffer.add_bytes acc (Bytes.sub buff 0 n); 
          mkReadInProgress fd (FCons(Line (max (m-n) 1,buff,acc),rest))
        end else begin
          let one_line = Buffer.contents acc in
          Buffer.clear acc;
          mkReadInProgress fd (rest (Ok one_line))
        end
      with Unix.Unix_error _ as e ->
        Buffer.clear acc;
        mkReadInProgress fd (rest (Error e))
    end
  | ReadInProgress(fd,FCons(Bytes(n,buff),rest)) ->
    let ready_fds = List.filter ((<>) fd) ready_fds in
    ready_fds,
    begin try
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
      