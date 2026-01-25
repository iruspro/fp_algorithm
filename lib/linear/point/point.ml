type t = { point : Q.t list (* [r_n; ...; r_1] *); dim : int }

(* Constructors *)
let from_list list = { point = List.rev list; dim = List.length list }

let from_array arr =
  { point = List.rev (Array.to_list arr); dim = Array.length arr }

(* Getters *)
let as_list { point; _ } = List.rev point
let as_reversed_list { point; _ } = point
let dim { dim; _ } = dim

(* Functions *)
let extend_dim point v = { point = v :: point.point; dim = succ point.dim }

(* Print *)
let to_string point =
  let rec aux acc = function
    | [] -> acc ^ ")"
    | [ r ] -> aux (acc ^ Q.to_string r) []
    | r :: rs -> aux (acc ^ Q.to_string r ^ ", ") rs
  in
  aux "(" (as_list point)

let print point = print_string (to_string point)
