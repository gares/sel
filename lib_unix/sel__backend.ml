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

open Sel__unix_fd

(* The Unix backend: select on file descriptors plus a self pipe used
   to wake up select when a promise is resolved (possibly by another thread).

   The pipe is blocking, since on Windows set_nonblock works only on sockets.
   [pending] tells if a byte is (or is about to be) in the pipe: [notify]
   writes only if there is none, and [select] reads it, hence the pipe
   contains at most one byte: writing never blocks, and reading never blocks
   since the pipe is readable.
   A [notify] skipped because [pending] is set but [select] already read the
   byte is not lost: the promise was resolved before [notify] and is checked
   after [select] returns. *)
let self_pipe_r, self_pipe_w = Unix.pipe ~cloexec:true ()
let pending = Atomic.make false

let notify () =
  if Atomic.compare_and_set pending false true then
    try ignore (Unix.write_substring self_pipe_w "x" 0 1)
    with Unix.Unix_error _ -> Atomic.set pending false

let consume r =
  let b = Bytes.create 1 in
  (try ignore (Unix.read r b 0 1) with Unix.Unix_error _ -> ());
  Atomic.set pending false

let select readables ~timeout =
  let fds = List.filter_map (function Readable fd -> Some fd | _ -> None) readables in
  let ready_fds, _, _ =
    try Unix.select (self_pipe_r :: fds) [] [] timeout
    with Unix.Unix_error (Unix.EINTR, _, _) -> [], [], [] in
  if List.mem self_pipe_r ready_fds then consume self_pipe_r;
  List.filter (function Readable fd -> List.mem fd ready_fds | _ -> false) readables

let gettimeofday = Unix.gettimeofday
