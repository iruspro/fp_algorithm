open Point

(* Constructors *)
let%test "from_list: dim and order" =
  let p = from_list [ Q.one; Q.of_float 0.33; Q.of_int 3 ] in
  dim p = 3 && as_list p = [ Q.one; Q.of_float 0.33; Q.of_int 3 ]

let%test "from_array: dim and order" =
  let p = from_array [| Q.one; Q.of_float 0.33; Q.of_int 3 |] in
  dim p = 3 && as_list p = [ Q.one; Q.of_float 0.33; Q.of_int 3 ]

(* Getters *)
let%test "as_list vs as_reversed_list" =
  let p = from_list [ Q.one; Q.of_float 0.33; Q.of_int 3 ] in
  as_list p = [ Q.one; Q.of_float 0.33; Q.of_int 3 ]
  && as_reversed_list p = [ Q.of_int 3; Q.of_float 0.33; Q.one ]

let%test "dim" =
  dim (from_list []) = 0
  && dim (from_array [| Q.one |]) = 1
  && dim (from_list [ Q.of_int 3; Q.of_float 0.33; Q.one ]) = 3

(* Functions *)
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
