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

(* Sel_core.System_event.file_descr: it is defined here, and not in Sel_core, since
   the backend uses it, and Sel_core uses the backend. The implementations of sel
   extend it in their private modules *)
type t = ..
