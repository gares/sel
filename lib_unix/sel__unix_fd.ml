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

(* The file descriptors of the Unix events, known by the backend. It is
   not in Sel_unix since Sel_unix depends on Sel, which depends on the
   backend *)
type Sel__fd.t += Readable of Unix.file_descr
