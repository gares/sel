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
   not in Sel since Sel depends on Sel_core, which depends on the
   backend *)
type Sel_core__fd.t += Readable of Unix.file_descr
