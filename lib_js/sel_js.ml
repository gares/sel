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


let wakeup = Sel_js__driver.wakeup

let run ?budget ?poll ~handle_event ~init () =
  Sel_js__driver.run ~pop_ready:Sel_core.pop_ready ~add:Sel_core.Todo.add
    ?budget ?poll ~handle_event ~init ()
