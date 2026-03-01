type t = { coords : Q.t list (* [r_n; ...; r_1] *) }

(* CONSTRUCTORS *)
let from_list coords = { coords = List.rev coords }
let from_rev_list coords = { coords }

(* GETTERS *)
let as_list point = List.rev point.coords
let as_reversed_list point = point.coords
let dim point = List.length point.coords

(* FUNCTIONS *)
let extend_dim point coord = from_rev_list (coord :: as_reversed_list point)

(* PRINT *)
let to_string point =
  let rec aux acc = function
    | [] -> acc ^ ")"
    | [ r ] -> aux (acc ^ Q.to_string r) []
    | r :: rs -> aux (acc ^ Q.to_string r ^ ", ") rs
  in
  aux "(" (as_list point)

let print point = print_string (to_string point)

(* TESTS *)
(* CONSTRUCTORS *)
let%test "from_list: dim and order" =
  let p = from_list [ Q.one; Q.of_float 0.33; Q.of_int 3 ] in
  dim p = 3 && as_list p = [ Q.one; Q.of_float 0.33; Q.of_int 3 ]

(* GETTERS *)
let%test "as_list vs as_reversed_list" =
  let p = from_list [ Q.one; Q.of_float 0.33; Q.of_int 3 ] in
  as_list p = [ Q.one; Q.of_float 0.33; Q.of_int 3 ]
  && as_reversed_list p = [ Q.of_int 3; Q.of_float 0.33; Q.one ]

let%test "dim" =
  dim (from_list []) = 0
  && dim (from_list [ Q.one ]) = 1
  && dim (from_list [ Q.of_int 3; Q.of_float 0.33; Q.one ]) = 3

(* FUNCTIONS *)
let%test "extend_dim increases dim" =
  let p = from_list [ Q.one; Q.one ] in
  let p' = extend_dim p (Q.of_int 3) in
  dim p' = 3

let%test "extend_dim adds element in front (logical end)" =
  let p = from_list [ Q.one; Q.one ] in
  let p' = extend_dim p (Q.of_int 3) in
  as_list p' = [ Q.one; Q.one; Q.of_int 3 ]

let%test "extend_dim on empty point" =
  let p = from_list [] in
  let p' = extend_dim p Q.one in
  dim p' = 1 && as_list p' = [ Q.one ]

(* PRINT *)
let%test "to_string empty" =
  let p = from_list [] in
  to_string p = "()"

let%test "to_string 1-dim" =
  let p = from_list [ Q.of_float 2.5 ] in
  to_string p = "(5/2)"

let%test "to_string 2-dim" =
  let p = from_list [ Q.of_int 1; Q.of_float 2.5 ] in
  to_string p = "(1, 5/2)"
