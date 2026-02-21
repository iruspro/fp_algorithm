open Lin_expr

(* CONSTRUCTORS AND GETTERS *)
let%test "from_list_as_list" =
  let coeffs = [ Q.of_int 2; Q.of_int 3; Q.of_int 4 ] in
  let expr = from_list coeffs in
  dim expr = 2 && as_list expr = coeffs

let%test "dim" =
  let expr1 = from_list [ Q.of_int 1; Q.of_int 2; Q.of_int 3; Q.of_int 1 ]
  and expr2 = from_list [ Q.zero ] in
  dim expr1 = 3 && dim expr2 = 0

(* SPECIAL EXPRESSIONS *)
let%test "zero" =
  let expr = zero 2 and result = from_list [ Q.zero; Q.zero; Q.zero ] in
  equal expr result

let%test "one" =
  let expr = one 2 and result = from_list [ Q.zero; Q.zero; Q.one ] in
  equal expr result

let%test "x" =
  let expr = x 3 1 and result = from_list [ Q.zero; Q.zero; Q.one; Q.zero ] in
  equal expr result

(* OPERATORS *)
let%test "equal" =
  let expr1 = from_list [ Q.of_int 1; Q.of_int 2; Q.of_int 3 ]
  and expr2 = from_list [ Q.of_int 1; Q.of_int 2; Q.of_int 3 ]
  and expr3 = from_list [ Q.of_int 1; Q.of_int 2; Q.of_int 4 ] in
  equal expr1 expr2 && not (equal expr1 expr3)

let%test "add" =
  (* (y + x + 1) + (y + 2x + 3) = 2y + 3x + 4 *)
  let expr1 = from_list (List.init 3 (fun _ -> Q.of_int 1))
  and expr2 = from_list [ Q.of_int 1; Q.of_int 2; Q.of_int 3 ] in
  let expr = add expr1 expr2
  and result = from_list [ Q.of_int 2; Q.of_int 3; Q.of_int 4 ] in
  equal expr result

let%test "mul_by" =
  (* 10 * (y + 2x + 3) = 10y + 20x + 30 *)
  let expr =
    mul_by (Q.of_int 10) (from_list [ Q.of_int 1; Q.of_int 2; Q.of_int 3 ])
  and result = from_list [ Q.of_int 10; Q.of_int 20; Q.of_int 30 ] in
  equal expr result

let%test "sub" =
  (* (10y - 5x + 3) - (-7.5x + 4) = 10y + 2.5x - 1 *)
  let expr1 = from_list [ Q.of_int 10; Q.of_int (-5); Q.of_int 3 ]
  and expr2 = from_list [ Q.zero; Q.of_float (-7.5); Q.of_int 4 ] in
  let expr = sub expr1 expr2
  and result = from_list [ Q.of_int 10; Q.of_float 2.5; Q.minus_one ] in
  equal expr result

(* FUNCTIONS *)
let%test "eval" =
  (* 0 at random point *)
  let expr1 = zero 5
  and point1 =
    Point.from_list (List.init 5 (fun _ -> Q.of_float (Random.float 100.)))
  and result1 = Q.zero in
  (* y + 2x + 3 at (2; 10.5)*)
  let expr2 = from_list [ Q.of_int 1; Q.of_int 2; Q.of_int 3 ]
  and point2 = Point.from_list [ Q.of_int 2; Q.( // ) 21 2 ]
  and result2 = Q.( // ) 35 2 in
  (* 0*x + 10 at random point *)
  let expr3 = from_list [ Q.of_int 0; Q.of_int 10 ]
  and point3 =
    Point.from_list (List.init 1 (fun _ -> Q.of_float (Random.float 100.)))
  and result3 = Q.of_int 10 in
  Q.equal (eval expr1 point1) result1
  && Q.equal (eval expr2 point2) result2
  && Q.equal (eval expr3 point3) result3

let%test "sub_last" =
  (* 5 y + 2 x + 1 with 2y + 2x + 5 -> 10 y + 12x + 26 *)
  let expr1 = from_list [ Q.of_int 5; Q.of_int 2; Q.of_int 1 ]
  and expr2 = from_list [ Q.of_int 2; Q.of_int 2; Q.of_int 5 ] in
  let expr = sub_last expr1 expr2
  and result = from_list [ Q.of_int 10; Q.of_int 12; Q.of_int 26 ] in
  equal expr result

let%test "reduce_dim" =
  (* 7y + 5x + 1 -> 5x + 1 *)
  let expr = from_list [ Q.of_int 7; Q.of_int 5; Q.of_int 1 ]
  and result = from_list [ Q.of_int 5; Q.of_int 1 ] in
  equal (reduce_dim expr) result

let%test "extend_dim" =
  let expr = from_list [ Q.of_int 7; Q.of_int 5; Q.of_int 1 ]
  and v = Q.of_float 0.5 in
  let extended = extend_dim expr v
  and result =
    from_list [ Q.of_float 0.5; Q.of_int 7; Q.of_int 5; Q.of_int 1 ]
  in
  equal extended result && dim extended = dim expr + 1

let%test "find_supremum_dim" =
  let result = from_list [ Q.of_float 2.4; Q.of_float 2.4; Q.of_float 2.4 ] in
  let terms =
    [
      from_list [ Q.of_float 2.5; Q.of_float 2.5; Q.of_float 2.5 ];
      from_list [ Q.of_float 2.75; Q.of_float 2.75; Q.of_float 2.75 ];
      from_list [ Q.of_float 2.45; Q.of_float 2.45; Q.of_float 2.45 ];
      result;
      from_list [ Q.of_float 2.5; Q.of_float 2.5; Q.of_float 2.5 ];
      from_list [ Q.of_float 2.75; Q.of_float 2.75; Q.of_float 2.75 ];
      from_list [ Q.of_float 2.45; Q.of_float 2.45; Q.of_float 2.45 ];
    ]
  and point = Point.from_list [ Q.of_int 2; Q.of_int 2 ] in
  match find_supremum_term terms point with
  | Some term -> equal term result
  | None -> false

(* PRINT *)
let%test "to_string" =
  let expr1 = from_list [ Q.zero; Q.zero; Q.of_float (-2.5) ]
  and result1 = "-5/2"
  and expr2 = from_list [ Q.minus_one; Q.zero; Q.of_float 2.5 ]
  and result2 = "-x₂ + 5/2"
  and expr3 =
    from_list [ Q.of_float 2.5; Q.zero; Q.of_int 1; Q.of_int (-2); Q.of_int 3 ]
  and result3 = "5/2 x₄ + x₂ - 2x₁ + 3" in
  to_string expr1 = result1
  && to_string expr2 = result2
  && to_string expr3 = result3
