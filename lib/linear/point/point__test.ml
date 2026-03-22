open Point

(* CONSTRUCTORS *)
let%test "from_list: dim and order" =
  let p = from_list [ Q.one; Q.of_float 0.33; Q.of_int 3 ] in
  dim p = 3 && as_list p = [ Q.one; Q.of_float 0.33; Q.of_int 3 ]

let%test "from_list: empty" =
  let p = from_list [] in
  dim p = 0 && as_list p = []

let%test "from_list: single element" =
  let p = from_list [ Q.of_int 5 ] in
  dim p = 1 && as_list p = [ Q.of_int 5 ]

let%test "from_list: roundtrip" =
  let coords = [ Q.one; Q.of_float 0.33; Q.of_int 3 ] in
  as_list (from_list coords) = coords

let%test "from_rev_list: order" =
  let p = from_rev_list [ Q.of_int 3; Q.of_float 0.33; Q.one ] in
  as_list p = [ Q.one; Q.of_float 0.33; Q.of_int 3 ]

let%test "from_rev_list: empty" =
  let p = from_rev_list [] in
  dim p = 0 && as_reversed_list p = []

let%test "from_rev_list: roundtrip" =
  let coords = [ Q.of_int 3; Q.of_float 0.33; Q.one ] in
  as_reversed_list (from_rev_list coords) = coords

(* GETTERS *)
let%test "as_reversed_list" =
  let p = from_list [ Q.one; Q.of_float 0.33; Q.of_int 3 ] in
  as_reversed_list p = [ Q.of_int 3; Q.of_float 0.33; Q.one ]

let%test "dim" =
  dim (from_list []) = 0
  && dim (from_list [ Q.one ]) = 1
  && dim (from_list [ Q.of_int 3; Q.of_float 0.33; Q.one ]) = 3

(* FUNCTIONS *)
let%test "extend_dim: increases dim" =
  let p = from_list [ Q.one; Q.one ] in
  dim (extend_dim p (Q.of_int 3)) = 3

let%test "extend_dim: new coord is last" =
  let p = from_list [ Q.one; Q.one ] in
  as_list (extend_dim p (Q.of_int 3)) = [ Q.one; Q.one; Q.of_int 3 ]

let%test "extend_dim: does not mutate original" =
  let p = from_list [ Q.one; Q.one ] in
  let _ = extend_dim p (Q.of_int 3) in
  dim p = 2 && as_list p = [ Q.one; Q.one ]

let%test "extend_dim: on empty point" =
  let p = from_list [] in
  let p' = extend_dim p Q.one in
  dim p' = 1 && as_list p' = [ Q.one ]

(* PRINT *)
let%test "to_string: empty" = to_string (from_list []) = "()"
let%test "to_string: 1-dim" = to_string (from_list [ Q.of_float 2.5 ]) = "(5/2)"

let%test "to_string: 2-dim" =
  to_string (from_list [ Q.of_int 1; Q.of_float 2.5 ]) = "(1, 5/2)"

let%test "to_string: 3-dim" =
  to_string (from_list [ Q.of_int 1; Q.of_int 2; Q.of_int 3 ]) = "(1, 2, 3)"
