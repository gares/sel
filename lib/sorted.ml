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

type priority = { user: int; insertion: int}
[@@deriving show]

let cmp_priority { user = u1; insertion = i1} {user=u2; insertion=i2} =
  let c = u1 - u2 in
  if c = 0 then i1 - i2 else c
  
let max_priority = { user = max_int; insertion = max_int }

let min_priority p1 p2 = if cmp_priority p1 p2 < 0 then p1 else p2

let default_priority = { user = 0; insertion = 0 }

let eq_user { user = u1; _} {user = u2; _} = u1 = u2

let lt_priority p1 p2 = cmp_priority p1 p2 < 0

let le_user { user = u1; _} {user = u2; _} = u1 <= u2


type 'a t = {
  sorted : bool;  
  data : ('a * priority) list;
}
[@@deriving show]

type 'a view =
  | Nil
  | Cons of ('a * priority) * 'a t

let nil = { sorted = true; data = [] }

let is_nil x = x.data = []

let length x = List.length x.data

let on_fst f = (); fun (x,_) -> f x
let on_snd2 f = (); fun (_,x) (_,y) -> f x y

let filter f l = { sorted = l.sorted; data = List.filter (on_fst f) l.data }

let rec map_filter f = function
| [] -> []
| x :: xs ->
    match f x with
    | None -> map_filter f xs
    | Some x -> x :: map_filter f xs
let map_filter f l = map_filter (on_fst f) l.data

let sort l =
  if l.sorted then l
  else { sorted = true; data = List.sort (on_snd2 cmp_priority) l.data }

let to_list l = List.map fst (sort l).data

let for_all f l = List.for_all (on_fst f) l.data

let look l =
  let l = sort l in
  match l.data with
  | [] -> Nil
  | x :: xs -> Cons(x,{ sorted = true; data = xs})

let min l =
  let l = sort l in
  match l.data with
  | [] -> max_priority, l
  | (_,p) :: _ -> p, l

let cons x p l =
  match l.data with
  | (_,q) :: _ when l.sorted && lt_priority p q -> { sorted = true; data = (x,p) :: l.data }
  | _ -> { sorted =false; data = (x,p) :: l.data }
let cons_opt = function
  | Some(x,p) -> cons x p
  | None -> fun x -> x

let append l1 l2 = { sorted = false; data = l1.data @ l2.data }
let concat l = { sorted = false; data = List.concat (List.map (fun x -> x.data) l) }
  
let of_list l = { sorted = false; data = l }

let partition f { sorted; data } =
  let rev = if sorted then List.rev else fun x -> x in
  let rec aux yes no = function
    | [] -> { sorted; data = rev yes }, { sorted; data = rev no }
    | x :: xs ->
        if f (fst x)
        then aux (x :: yes) no xs
        else aux yes (x :: no) xs
  in
    aux [] [] data

