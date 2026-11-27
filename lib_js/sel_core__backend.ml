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


(* The JavaScript backend: one cannot block, there are no file descriptors,
   and resolving a promise wakes up the driver of Sel_js.run *)
let select _ ~timeout =
  if timeout > 0. then
    failwith "Sel_core: cannot block in JavaScript, use Sel_js.run or Sel_core.pop_ready";
  []

let gettimeofday = Sel_js__driver.now
let notify = Sel_js__driver.wakeup
