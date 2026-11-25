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

module Sorted = Sel__sorted

(* System events are provided by extensions (e.g. the Unix one) *)
module System_event = struct

  type file_descr = Sel__fd.t = ..

  type ('a,'b) step =
    | Yes of 'a
    | No  of 'b
    | Advanced of 'b

  type 'a t = {
    fd : file_descr option;
    advance : unit -> ('a, 'a t) step;
    descr : string;
  }

  let rec map f { fd; advance; descr } =
    let advance () =
      match advance () with
      | Yes x -> Yes (f x)
      | No e -> No (map f e)
      | Advanced e -> Advanced (map f e) in
    { fd; advance; descr }

  let pp _ fmt { descr; _ } = Format.fprintf fmt "%s" descr

end

type 'a system_event = 'a System_event.t
let pp_system_event = System_event.pp

module Promise = struct

  type 'a state =
    | Fulfilled of 'a
    | Rejected of exn [@printer fun fmt e -> Format.fprintf fmt "%s" (Printexc.to_string e)]
  [@@deriving show]

  type 'a t = 'a state option Atomic.t

  let pp pp_a fmt p =
    match Atomic.get p with
    | None -> Format.fprintf fmt "<pending>"
    | Some s -> pp_state pp_a fmt s

  type 'a handler = 'a t

  let resolve p s =
    if Atomic.compare_and_set p None (Some s) then Sel__backend.notify ()
    else raise @@ Failure "cannot resolve a promise twice"

  let make () : 'a t * 'a handler =
    let p = Atomic.make None in
    p, p
  
  let is_resolved p = Atomic.get p <> None
  let get p =
    match Atomic.get p with
    | Some x -> x
    | None -> raise @@ Failure "promise not resolved"

  let fulfill r x = resolve r (Fulfilled x)

  let reject r x = resolve r (Rejected x)
end

type 'a queue_event =
  | WaitQueue1 : 'b Queue.t * ('b -> 'a) -> 'a queue_event
  | WaitQueueBatch1 : 'b Queue.t * ('b -> 'b list -> 'a) -> 'a queue_event
  | WaitQueue2 : 'b Queue.t * 'c Queue.t * ('b -> 'c -> 'a) -> 'a queue_event
  | WaitPromise : 'b Promise.t * ('b Promise.state -> 'a) -> 'a queue_event

let pp_queue_event _ fmt = function
  | WaitQueue1 _ -> Stdlib.Format.fprintf fmt "WaitQueue1"
  | WaitQueueBatch1 _ -> Stdlib.Format.fprintf fmt "WaitQueueBatch1"
  | WaitQueue2 _ -> Stdlib.Format.fprintf fmt "WaitQueue2"
  | WaitPromise _ -> Stdlib.Format.fprintf fmt "WaitPromise"

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
  | Task of ('a -> 'a -> bool) option * 'a task_event [@print fun f fmt (_,x) -> pp_task_event f fmt x]
  [@@deriving show]

let map_system_event = System_event.map
  
let map_queue_event f = function
  | WaitQueue1(q1,k) -> WaitQueue1(q1,(fun x -> f (k x)))
  | WaitQueueBatch1(q1,k) -> WaitQueueBatch1(q1,(fun x xs -> f (k x xs)))
  | WaitQueue2(q1,q2,k) -> WaitQueue2(q1,q2,(fun x y -> f (k x y)))
  | WaitPromise(p,k) -> WaitPromise(p,(fun x -> f (k x)))

let map_task_event f x = f x

let map f = function
  | Task(_,e) -> Task (None, map_task_event f e)
  | SystemEvent e -> SystemEvent (map_system_event f e)
  | QueueEvent e -> QueueEvent(map_queue_event f e)  
  
type 'a t = 'a event_ WithAttributes.t
[@@deriving show]
  
let map f e = WithAttributes.map (map f) e

let get_cancellation_handle { WithAttributes.cancelled; _ } = cancelled
let cancel x = x := true

end

open Event

type ('a,'b) has_finished = ('a,'b) System_event.step =
  | Yes of 'a
  | No  of 'b
  | Advanced of 'b

module On = struct

  type 'a res = ('a,exn) result

let queue ?priority ?name q1 k : 'a Event.t =
  make_event ?priority ?name @@ QueueEvent (WaitQueue1(q1,k))

let queue_all ?priority ?name q1 k : 'a Event.t =
  make_event ?priority ?name @@ QueueEvent (WaitQueueBatch1(q1,k))
  
let queues ?priority ?name q1 q2 k : 'a Event.t =
  make_event?priority ?name  @@ QueueEvent (WaitQueue2(q1,q2,k))

let promise ?priority ?name (p : 'a Promise.t) (k : 'a Promise.state -> 'b) : 'b Event.t =
  make_event ?priority ?name @@ QueueEvent (WaitPromise(p,k))

end

let now ?priority ?name ?undup task : 'a Event.t =
  make_event ?priority ?name @@ Task (undup,task)
  
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
  | WaitPromise(p,k) as x ->
      match Atomic.get p with
      | None -> (), No x
      | Some s -> (), Yes (k s)

(* A system event is advanced only if it has no file descriptor (it is
   polled) or if its file descriptor is ready. Once a file descriptor is
   used by an event, it is consumed (it is removed from the ready list). This
   makes us resilient to multiple events waiting on the same descriptor *)
let advance_system ready_fds _ ({ System_event.fd; advance; _ } as x) =
  match fd with
  | None -> ready_fds, advance ()
  | Some fd when not (List.mem fd ready_fds) -> ready_fds, No x
  | Some fd ->
    let ready_fds = List.filter ((<>) fd) ready_fds in
    ready_fds, advance ()
