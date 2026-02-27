type t = { coords : Q.t list (* [r_n; ...; r_1] *) }

(* CONSTRUCTORS *)
let from_list coords = { coords = List.rev coords }
let from_reversed_list coords = { coords }

(* GETTERS *)
let as_list point = List.rev point.coords
let as_reversed_list point = point.coords
let dim point = List.length point.coords

(* FUNCTIONS *)
let extend_dim point coord = from_reversed_list (coord :: as_reversed_list point)

(* PRINT *)
let to_string point =
  let rec aux acc = function
    | [] -> acc ^ ")"
    | [ r ] -> aux (acc ^ Q.to_string r) []
    | r :: rs -> aux (acc ^ Q.to_string r ^ ", ") rs
  in
  aux "(" (as_list point)

let print point = print_string (to_string point)
