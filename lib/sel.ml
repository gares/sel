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

open Events
open Event

let drop_event_type : 'b -> 'a WithAttributes.t -> 'b WithAttributes.t =
  fun b e -> WithAttributes.map (fun _ -> b) e
  
(* Splits the events intro 3 lists: systm, queue and task *)
let partition_events l =
  let rec partition_events sys queue task = function
    | [] -> sys, queue, task
    | { WithAttributes.it = SystemEvent x; _ } as e :: rest ->
        partition_events ((drop_event_type x e,e.priority) :: sys) queue task rest
    | { WithAttributes.it = QueueEvent x; _ } as e :: rest ->
        partition_events sys ((drop_event_type x e,e.priority) :: queue) task rest
    | { WithAttributes.it = Task (u,x); _ } as e :: rest ->
        partition_events sys queue ((Option.map (fun f x y -> f x.WithAttributes.it y.WithAttributes.it) u, drop_event_type x e, e.priority) :: task) rest
  in
    partition_events [] [] [] l

let next_deadline delta = Unix.gettimeofday() +. delta

module Todo = struct

type 'a t = {
  (* The pop API may need to postpone ready events *)
  ready : 'a WithAttributes.t Sorted.t;
  (* The three queues of events *)
  system : ('a system_event) WithAttributes.t Sorted.t;
  queue  : ('a queue_event) WithAttributes.t Sorted.t;
  tasks  : ('a task_event) WithAttributes.t Sorted.t;
}
[@@deriving show]
let empty = {
  system = Sorted.nil;
  queue = Sorted.nil ;
  tasks = Sorted.nil;
  ready = Sorted.nil;
}

let prune_cancelled { system; queue; tasks; ready } =
  let not_cancelled { WithAttributes.cancelled; _ } = !cancelled = false in
  let system = Sorted.filter not_cancelled system in
  let queue  = Sorted.filter not_cancelled queue in
  let tasks  = Sorted.filter not_cancelled tasks in
  let ready  = Sorted.filter not_cancelled ready in
  { system; queue; tasks; ready }

let size todo =
  let { system; queue; tasks; ready } = prune_cancelled todo in
  Sorted.length system + Sorted.length queue + Sorted.length tasks + Sorted.length ready

let is_empty todo =
  let { system; queue; tasks; ready } = prune_cancelled todo in
  Sorted.is_nil system &&
  Sorted.is_nil queue &&
  Sorted.is_nil tasks &&
  Sorted.is_nil ready

(* In order to preserve insertion order we tag each event with a tick *)
let tick = ref 0

let add { system; queue; tasks; ready } l =
  let tick ({ WithAttributes.priority; _ } as e) =
    incr tick;
    let priority = { priority with insertion = !tick } in
    { e with priority } in
  let l = List.map tick l in
  let new_sys, new_queue, new_tasks = partition_events l in
  { 
    ready;
    system = Sorted.append system (Sorted.of_list new_sys);
    queue  = Sorted.append queue (Sorted.of_list new_queue);
    tasks  = Sorted.append_uniq tasks new_tasks;
  }
  
end

(* Like List.filter but also returns the minimum priority of ready events.
   Moreover ~advance can make the event advance (whilst not being ready yet)*)
let pull_ready ~advance st l =
  let rec pull_ready yes min_priority_ready no st l =
    match Sorted.look l with
    | Sorted.Nil -> yes, no, min_priority_ready
    | Sorted.Cons(({ WithAttributes.it; cancelled; priority; _ } as e, _), rest) ->
        match advance st cancelled it with
        | st, Yes y ->
          let min_priority_ready = Sorted.min_user min_priority_ready priority in
          let e = drop_event_type y e in
          pull_ready (Sorted.cons e e.priority yes) min_priority_ready no st rest
        | st, Advanced x  ->
          pull_ready yes min_priority_ready (Sorted.cons { e with it = x } e.priority no) st rest 
        | st, No x  ->
          pull_ready yes min_priority_ready (Sorted.cons { e with it = x } e.priority no) st rest 
  in
    pull_ready Sorted.nil Sorted.max_priority Sorted.nil st l
  
type ('a,'b) ev_checker =
  'a WithAttributes.t Sorted.t -> 'b WithAttributes.t Sorted.t * 'a WithAttributes.t Sorted.t * Sorted.priority

let file_descriptors_of l =
  Sorted.map_filter (function { WithAttributes.it = ReadInProgress(fd,_); _ } -> Some fd | _ -> None) l

let filter_file_descriptor fds = function
  | { WithAttributes.it = ReadInProgress(fd,_); _ } -> List.mem fd fds
  | _ -> false

(* For fairness reasons, even if there are immediately ready events we
   give a shot to system events with 0 wait, otherwise we wait until a
   system event is ready. We never sleep forever, since process death events
   do not wakeup select: we anyway wake up 10 times per second *)
(* After advancing once, check if any waiting tasks have lower priority (more important) than the lowest
   ready queue, task or system event. If so, try to advance the system task event.
   The result is that it when reading 'n' bytes, it is no longer necessary to interleave up to 'n' ready tasks.
*)
let check_for_system_events min_prio_task_queue : ('a system_event,'a) ev_checker = fun waiting ->
  let rec check_for_system_events new_ready waiting_skipped min_prio_ready waiting =
    let fds = file_descriptors_of waiting in
    let ready_fds, _, _ = Unix.select fds [] [] 0.0 in
    let new_ready_1, waiting, min_prio_ready_1 = pull_ready ~advance:advance_system ready_fds waiting in
    let new_ready = Sorted.append new_ready_1 new_ready in
    let min_prio_ready = Sorted.min_user min_prio_ready_1 min_prio_ready in
    if ready_fds = [] then
      new_ready, Sorted.append waiting waiting_skipped, min_prio_ready
    else
      let waiting, waiting_skipped_1 = Sorted.partition (filter_file_descriptor ready_fds) waiting in
      let waiting_skipped = Sorted.concat [waiting_skipped_1; waiting_skipped] in
      check_for_system_events new_ready waiting_skipped min_prio_ready waiting
  in
    let waiting, waiting_skipped = Sorted.partition_priority (fun x -> Sorted.le_user x min_prio_task_queue) waiting in
    check_for_system_events Sorted.nil waiting_skipped Sorted.max_priority waiting

let check_for_queue_events : ('a queue_event,'a) ev_checker =
  fun waiting ->
    let new_ready, waiting, min_prio = pull_ready ~advance:advance_queue () waiting in
    new_ready, waiting, min_prio  

(* This is blocking wait (modulo a deadline). We check for system events
   (io, process death) or a queue (in case some thread puts a token there). *)
let rec wait_for_system_or_queue_events ~deadline (fds,sys) queue =
  if Unix.gettimeofday () > deadline then Sorted.nil, Sorted.nil, sys, queue, Sorted.max_priority
  else
    let ready_fds, _, _ = Unix.select fds [] [] 0.1 in
    let ready_sys, waiting_sys, min_prio_sys = pull_ready ~advance:advance_system ready_fds sys in
    let ready_queue, waiting_queue, min_prio_queue = pull_ready ~advance:advance_queue () queue in
    if not(Sorted.is_nil ready_sys) || not(Sorted.is_nil ready_queue)
    then
      let min_prio =  Sorted.min_priority min_prio_queue min_prio_sys in
      let new_ready_sys, waiting_sys, min_prio_new_ready_sys = check_for_system_events min_prio waiting_sys in
      Sorted.append new_ready_sys ready_sys, ready_queue, waiting_sys, waiting_queue, Sorted.min_priority min_prio_new_ready_sys min_prio
    else wait_for_system_or_queue_events ~deadline (fds,waiting_sys) queue

let wait_for_system_or_queue_events ~deadline sys queue =
  let fds = file_descriptors_of sys in
  wait_for_system_or_queue_events ~deadline (fds,sys) queue

let rec pull_tasks min_prio l =
  match Sorted.look l with
  | Sorted.Nil -> Sorted.nil, Sorted.nil
  | Sorted.Cons((x,p),l) when Sorted.le_user p min_prio ->
      let tasks, l = pull_tasks min_prio l in
      Sorted.cons x p tasks, l
  | _ -> Sorted.nil, l
  
(* Keep only events with a user priority equal to the given one (assumed to be the minimum) *)
let postpone p ready =
  let leq_user_prio { WithAttributes.priority = q; _} = Sorted.le_user q p in
  let ready, postponed = Sorted.partition leq_user_prio ready in
  ready, postponed
        
let wait ?(deadline=max_float) todo : 'a WithAttributes.t list * 'a Todo.t =
  let open Todo in
  let { system; queue; tasks; ready } as todo = prune_cancelled todo in
  if is_empty todo then
    [], todo
  else
    let min_prio, ready = Sorted.min ready in
    let ready_queue, waiting_queue, min_prio_queue = check_for_queue_events queue in
    let min_prio = Sorted.min_priority min_prio min_prio_queue in
    let ready_sys, waiting_sys, min_prio_sys = check_for_system_events min_prio system in
    if Sorted.is_nil ready_sys &&
       Sorted.is_nil ready_queue &&
       Sorted.is_nil ready &&
       Sorted.is_nil tasks
    then
      let ready_sys, ready_queue, waiting_sys, waiting_queue, min_prio =
        wait_for_system_or_queue_events ~deadline waiting_sys waiting_queue in
      let ready_sys, postponed_sys = postpone min_prio ready_sys in
      let ready_queue, postponed_queue = postpone min_prio ready_queue in
      let postponed = Sorted.append postponed_sys postponed_queue in
      let ready = Sorted.to_list (Sorted.append ready_sys ready_queue) in
      ready, { system = waiting_sys; queue = waiting_queue; tasks; ready = postponed }
    else
      let min_prio = Sorted.min_priority min_prio min_prio_sys in
      let ready_old, postponed_ready = pull_tasks min_prio ready in
      let ready_tasks, tasks = pull_tasks min_prio tasks in
      let ready_sys, postponed_sys = postpone min_prio ready_sys in
      let ready_queue, postponed_queue = postpone min_prio ready_queue in
      let postponed = Sorted.concat [postponed_sys; postponed_queue; postponed_ready] in
      let ready = Sorted.to_list (Sorted.concat [ready_sys; ready_queue; ready_tasks; ready_old]) in
      ready, { system = waiting_sys; queue = waiting_queue; tasks; ready = postponed }

let pop_return (ready, todo) =
  match ready with
  | { WithAttributes.it; _} :: rest ->
    let rest_w_prio = List.map (fun x -> x, x.WithAttributes.priority) rest in
    let ready = Sorted.append todo.Todo.ready (Sorted.of_list rest_w_prio) in
    let todo = { todo with Todo.ready } in
    Some it, todo
  | [] ->
    None, todo

let pop l =
  match pop_return @@ wait l with
  | None, _ -> raise @@ Failure "nothing to pop"
  | Some x, t -> x, t
let pop_opt l = pop_return @@ wait l

let pop_timeout ~stop_after_being_idle_for:delta l = 
  let deadline = next_deadline delta in
  pop_return @@ wait ~deadline l

let wait_return (l,todo) =
  l |> List.map (fun x -> x.WithAttributes.it), todo

let wait_timeout ~stop_after_being_idle_for:delta l =
  let deadline = next_deadline delta in
  wait_return @@ wait ~deadline l

let wait l = wait_return @@ wait l

include Events
