(* TESTS *)
(* zero *)
(* let%test "zero" = equal (zero 2) [ Q.zero; Q.zero; Q.zero ]

(* one *)
let%test "one" = equal (one 2) [ Q.zero; Q.zero; Q.one ]

(* x *)
let%test "x" = equal (x 3 1) [ Q.zero; Q.zero; Q.one; Q.zero ]

(* eval *)
let%test "eval" =
  (* 0 at random point *)
  let expr1 = zero 5
  and point1 =
    Point.from_array (Array.init 5 (fun _ -> Q.of_int (Random.full_int 100)))
  in
  (* y + 2x + 3 at (2; 10.5)*)
  let expr2 = [ Q.of_int 1; Q.of_int 2; Q.of_int 3 ]
  and point2 = Point.from_array [| Q.of_int 2; Q.( // ) 21 2 |] in
  (* 0*x + 10 at random point *)
  let expr3 = [ Q.of_int 0; Q.of_int 10 ]
  and point3 =
    Point.from_array (Array.init 1 (fun _ -> Q.of_float (Random.float 100.)))
  in
  Q.equal (eval expr1 point1) Q.zero
  && Q.equal (eval expr2 point2) (Q.( // ) 35 2)
  && Q.equal (eval expr3 point3) (Q.of_int 10)

(* sub_last *)
let%test "sub_last" =
  (* 5 y + 2 x + 1 with 2y + 2x + 5 -> 10 y + 12x + 26 *)
  let expr1 = [ Q.of_int 5; Q.of_int 2; Q.of_int 1 ]
  and expr2 = [ Q.of_int 2; Q.of_int 2; Q.of_int 5 ] in
  equal (sub_last expr1 expr2) [ Q.of_int 10; Q.of_int 12; Q.of_int 26 ]

(* reduce_dim *)
let%test "reduce_dim" =
  (* 0*y + 5x + 1 -> 5x + 1 *)
  equal
    (reduce_dim [ Q.zero; Q.of_int 5; Q.of_int 1 ])
    [ Q.of_int 5; Q.of_int 1 ]

(* add *)
let%test "add" =
  (* (y + x + 1) + (y + 2x + 3) = 2y + 3x + 4 *)
  let expr1 = List.init 3 (fun _ -> Q.of_int 1)
  and expr2 = [ Q.of_int 1; Q.of_int 2; Q.of_int 3 ] in
  equal (add expr1 expr2) [ Q.of_int 2; Q.of_int 3; Q.of_int 4 ]

(* mul_by *)
let%test "mul_by" =
  (* 10 * (y + 2x + 3) = 10y + 20x + 30 *)
  let expr = [ Q.of_int 1; Q.of_int 2; Q.of_int 3 ] in
  equal (mul_by (Q.of_int 10) expr) [ Q.of_int 10; Q.of_int 20; Q.of_int 30 ] *)
