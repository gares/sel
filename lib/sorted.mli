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

type priority = { user: int; insertion: int }

val pp_priority : Format.formatter -> priority -> unit

val cmp_priority : priority -> priority -> int

val max_priority : priority

val min_priority : priority -> priority -> priority

val lt_priority : priority -> priority -> bool

val eq_user : priority -> priority -> bool
val le_user : priority -> priority -> bool
val min_user : priority -> priority -> priority

val default_priority : priority

type 'a t
type 'a view =
    | Nil
    | Cons of ('a * priority) * 'a t

val look : 'a t -> 'a view
val cons : 'a -> priority -> 'a t -> 'a t
val cons_opt : ('a * priority) option -> 'a t -> 'a t
val nil : 'a t
val is_nil : 'a t -> bool
val length : 'a t -> int
val filter : ('a -> bool) -> 'a t -> 'a t
val for_all : ('a -> bool) -> 'a t -> bool
val append : 'a t -> 'a t -> 'a t
val concat : 'a t list -> 'a t
val map_filter : ('a -> 'b option) -> 'a t -> 'b list
val min : 'a t -> priority * 'a t
val to_list : 'a t -> 'a list
val of_list : ('a * priority) list -> 'a t
val pp : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
val partition : ('a -> bool) -> 'a t -> 'a t * 'a t
val partition_priority : (priority -> bool) -> 'a t -> 'a t * 'a t
