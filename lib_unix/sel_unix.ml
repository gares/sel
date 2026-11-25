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

open Sel
type 'a res = 'a On.res

(* Events in progress *)
type _ in_progress =
  | Line : int * Bytes.t * Buffer.t -> string in_progress
  | Bytes : int * Bytes.t -> Bytes.t in_progress

(* Reified function composition *)
type _ fcomp =
  | FNil : 'a -> 'a fcomp
  | FCons : 'a in_progress * ('a res -> 'b fcomp) -> 'b fcomp

let finish_with (k : 'a res -> 'b) x = FNil(k x)
let (--) = fun x xs -> FCons(x,xs)
(* locally one does let (--?) x y = err k x y in  as a cons with error handling *)
let err : type a b c. (c res -> b) -> a in_progress -> (a -> b fcomp) -> b fcomp =
  fun k x xs ->
    let xs = function
      | Ok v -> xs v
      | Error e -> FNil(k (Error e)) in
    FCons(x,xs)

open Sel__unix_fd

let one_line ?(at_least=1) () = Line (at_least,Bytes.make at_least '0', Buffer.create (max 40 at_least))
let some_bytes n ?(buff=Bytes.create n) () = Bytes(n,buff)

(* advance is only called when fd is readable *)
let rec read_in_progress : 'a. Unix.file_descr -> 'a fcomp -> 'a System_event.t = fun fd fcomp ->
  { System_event.fd = Some (Readable fd); descr = "ReadInProgress"; advance = (fun () -> advance_read fd fcomp) }

and mk_read_in_progress : 'a. Unix.file_descr -> 'a fcomp -> ('a, 'a System_event.t) System_event.step = fun fd -> function
  | FCons _ as f -> System_event.Advanced (read_in_progress fd f)
  | FNil x -> System_event.Yes x

and advance_read : 'a. Unix.file_descr -> 'a fcomp -> ('a, 'a System_event.t) System_event.step = fun fd -> function
  | FNil _ -> assert false
  | FCons(Line (m,buff,acc),rest) ->
    begin try
      let n = Unix.read fd buff 0 m in
      if n = 0 then begin
        Buffer.clear acc;
        mk_read_in_progress fd (rest (Error End_of_file))
      end else
        let c = Bytes.get buff (n-1) in
        if c != '\n' then begin
          Buffer.add_bytes acc (Bytes.sub buff 0 n);
          mk_read_in_progress fd (FCons(Line (max (m-n) 1,buff,acc),rest))
        end else begin
          let one_line = Buffer.contents acc in
          Buffer.clear acc;
          mk_read_in_progress fd (rest (Ok one_line))
        end
      with Unix.Unix_error _ as e ->
        Buffer.clear acc;
        mk_read_in_progress fd (rest (Error e))
    end
  | FCons(Bytes(n,buff),rest) ->
    begin try
      let m = Unix.read fd buff (Bytes.length buff - n) n in
      if m = 0 then
        mk_read_in_progress fd (rest (Error End_of_file))
      else
        if m != n then
          mk_read_in_progress fd (FCons(Bytes(n-m,buff),rest))
        else
          mk_read_in_progress fd (rest (Ok buff))
      with Unix.Unix_error _ as e -> mk_read_in_progress fd (rest (Error e))
    end

let rec wait_process pid k : 'a System_event.t =
  { System_event.fd = None; descr = Printf.sprintf "WaitProcess %d" pid; advance = (fun () ->
      let rc, code = Unix.waitpid [Unix.WNOHANG] pid in
      if rc == 0 then System_event.No (wait_process pid k)
      else System_event.Yes (k code)) }

module On = struct

let line ?priority ?name fd k : 'a Event.t =
  System_event.event ?priority ?name @@ read_in_progress fd (one_line () -- finish_with k)

let bytes ?priority ?name fd n k : 'a Event.t =
  System_event.event ?priority ?name @@ read_in_progress fd (some_bytes n () -- finish_with k)

let death_of ?priority ?name ~pid k : 'a Event.t =
  System_event.event ?priority ?name @@ wait_process pid k

let an_ocaml_value (k : 'a res -> 'b) : 'b fcomp =
  let (--?) x y = err k x y in
  some_bytes Marshal.header_size ()
  --? (fun buff -> let data_size = Marshal.data_size buff 0 in
  some_bytes data_size ~buff:(Bytes.extend buff 0 data_size) ()
  --? (fun buff -> let value = Marshal.from_bytes buff 0 in
  finish_with k (Ok value)))
;;

let ocaml_value ?priority ?name fd k : 'a Event.t =
  System_event.event ?priority ?name @@ read_in_progress fd (an_ocaml_value k)

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
  System_event.event ?priority ?name @@ read_in_progress fd (an_httpcle k)

end
