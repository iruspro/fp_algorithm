type t = { point : Q.t list (* [r_n; ...; r_1] *); dim : int }

(* Constructors *)
let from_list coords = { point = List.rev coords; dim = List.length coords }
let from_reversed_list coords = { point = coords; dim = List.length coords }

(* Getters *)
let as_list { point; _ } = List.rev point
let as_reversed_list { point; _ } = point
let dim { dim; _ } = dim

(* Functions *)
let extend_dim point coord = from_reversed_list (coord :: as_reversed_list point)

let flip_last point =
  let coords = as_reversed_list point in
  match coords with
  | [] -> point
  | r_n :: rs -> from_reversed_list (Q.sub Q.one r_n :: rs)

(* Print *)
let to_string point =
  let rec aux acc = function
    | [] -> acc ^ ")"
    | [ r ] -> aux (acc ^ Q.to_string r) []
    | r :: rs -> aux (acc ^ Q.to_string r ^ ", ") rs
  in
  aux "(" (as_list point)

let print point = print_string (to_string point)
