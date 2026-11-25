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

(* The backend of Sel, internal: sel is a virtual library and this is its
   virtual module, implemented by sel.unix (based on Unix.select) and sel.js
   (for js_of_ocaml and wasm_of_ocaml). A program picks one by linking it,
   the default is sel.unix. Dune does not allow a virtual module to be
   private, hence its name (and the odoc stop below). *)

(**/**)

(** [select l ~timeout] returns the ready file descriptors among [l],
    waiting at most [timeout] seconds *)
val select : Sel__fd.t list -> timeout:float -> Sel__fd.t list

(** the current time, in seconds *)
val gettimeofday : unit -> float

(** called (possibly by another thread) when a promise is resolved: it
    should interrupt any [select] running *)
val notify : unit -> unit
