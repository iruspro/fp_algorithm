(* CONSTRUCTORS AND GETTERS *)
let%test "from_list_as_list" =
  let coeffs = [ Q.of_int 2; Q.of_int 3; Q.of_int 4 ] in
  let expr = Lin_expr.from_list coeffs in
  Lin_expr.dim expr = 2 && Lin_expr.as_list expr = coeffs

let%test "dim" =
  let expr1 =
    Lin_expr.from_list [ Q.of_int 1; Q.of_int 2; Q.of_int 3; Q.of_int 1 ]
  and expr2 = Lin_expr.from_list [ Q.zero ] in
  Lin_expr.dim expr1 = 3 && Lin_expr.dim expr2 = 0

(* SPECIAL EXPRESSIONS *)
let%test "zero" =
  let expr = Lin_expr.zero 2
  and result = Lin_expr.from_list [ Q.zero; Q.zero; Q.zero ] in
  Lin_expr.equal expr result

let%test "one" =
  let expr = Lin_expr.one 2
  and result = Lin_expr.from_list [ Q.zero; Q.zero; Q.one ] in
  Lin_expr.equal expr result

let%test "x" =
  let expr = Lin_expr.x 3 1
  and result = Lin_expr.from_list [ Q.zero; Q.zero; Q.one; Q.zero ] in
  Lin_expr.equal expr result

(* OPERATORS *)
let%test "equal" =
  let expr1 = Lin_expr.from_list [ Q.of_int 1; Q.of_int 2; Q.of_int 3 ]
  and expr2 = Lin_expr.from_list [ Q.of_int 1; Q.of_int 2; Q.of_int 3 ]
  and expr3 = Lin_expr.from_list [ Q.of_int 1; Q.of_int 2; Q.of_int 4 ] in
  Lin_expr.equal expr1 expr2 && not (Lin_expr.equal expr1 expr3)

let%test "add" =
  (* (y + x + 1) + (y + 2x + 3) = 2y + 3x + 4 *)
  let expr1 = Lin_expr.from_list (List.init 3 (fun _ -> Q.of_int 1))
  and expr2 = Lin_expr.from_list [ Q.of_int 1; Q.of_int 2; Q.of_int 3 ] in
  let expr = Lin_expr.add expr1 expr2
  and result = Lin_expr.from_list [ Q.of_int 2; Q.of_int 3; Q.of_int 4 ] in
  Lin_expr.equal expr result

let%test "mul_by" =
  (* 10 * (y + 2x + 3) = 10y + 20x + 30 *)
  let expr =
    Lin_expr.mul_by (Q.of_int 10)
      (Lin_expr.from_list [ Q.of_int 1; Q.of_int 2; Q.of_int 3 ])
  and result = Lin_expr.from_list [ Q.of_int 10; Q.of_int 20; Q.of_int 30 ] in
  Lin_expr.equal expr result

let%test "sub" =
  (* (10y - 5x + 3) - (-7.5x + 4) = 10y + 2.5x - 1 *)
  let expr1 = Lin_expr.from_list [ Q.of_int 10; Q.of_int (-5); Q.of_int 3 ]
  and expr2 = Lin_expr.from_list [ Q.zero; Q.of_float (-7.5); Q.of_int 4 ] in
  let expr = Lin_expr.sub expr1 expr2
  and result =
    Lin_expr.from_list [ Q.of_int 10; Q.of_float 2.5; Q.minus_one ]
  in
  Lin_expr.equal expr result

(* FUNCTIONS *)
let%test "eval" =
  (* 0 at random point *)
  let expr1 = Lin_expr.zero 5
  and point1 =
    Point.from_array (Array.init 5 (fun _ -> Q.of_float (Random.float 100.)))
  and result1 = Q.zero in
  (* y + 2x + 3 at (2; 10.5)*)
  let expr2 = Lin_expr.from_list [ Q.of_int 1; Q.of_int 2; Q.of_int 3 ]
  and point2 = Point.from_array [| Q.of_int 2; Q.( // ) 21 2 |]
  and result2 = Q.( // ) 35 2 in
  (* 0*x + 10 at random point *)
  let expr3 = Lin_expr.from_list [ Q.of_int 0; Q.of_int 10 ]
  and point3 =
    Point.from_array (Array.init 1 (fun _ -> Q.of_float (Random.float 100.)))
  and result3 = Q.of_int 10 in
  Q.equal (Lin_expr.eval expr1 point1) result1
  && Q.equal (Lin_expr.eval expr2 point2) result2
  && Q.equal (Lin_expr.eval expr3 point3) result3

let%test "sub_last" =
  (* 5 y + 2 x + 1 with 2y + 2x + 5 -> 10 y + 12x + 26 *)
  let expr1 = Lin_expr.from_list [ Q.of_int 5; Q.of_int 2; Q.of_int 1 ]
  and expr2 = Lin_expr.from_list [ Q.of_int 2; Q.of_int 2; Q.of_int 5 ] in
  let expr = Lin_expr.sub_last expr1 expr2
  and result = Lin_expr.from_list [ Q.of_int 10; Q.of_int 12; Q.of_int 26 ] in
  Lin_expr.equal expr result

let%test "reduce_dim" =
  (* 7y + 5x + 1 -> 5x + 1 *)
  let expr = Lin_expr.from_list [ Q.of_int 7; Q.of_int 5; Q.of_int 1 ]
  and result = Lin_expr.from_list [ Q.of_int 5; Q.of_int 1 ] in
  Lin_expr.equal (Lin_expr.reduce_dim expr) result

let%test "extend_dim" =
  let expr = Lin_expr.from_list [ Q.of_int 7; Q.of_int 5; Q.of_int 1 ]
  and v = Q.of_float 0.5 in
  let extended = Lin_expr.extend_dim expr v
  and result =
    Lin_expr.from_list [ Q.of_float 0.5; Q.of_int 7; Q.of_int 5; Q.of_int 1 ]
  in
  Lin_expr.equal extended result
  && Lin_expr.dim extended = Lin_expr.dim expr + 1

let%test "find_supremum_dim" =
  let result =
    Lin_expr.from_list [ Q.of_float 2.4; Q.of_float 2.4; Q.of_float 2.4 ]
  in
  let terms =
    [
      Lin_expr.from_list [ Q.of_float 2.5; Q.of_float 2.5; Q.of_float 2.5 ];
      Lin_expr.from_list [ Q.of_float 2.75; Q.of_float 2.75; Q.of_float 2.75 ];
      Lin_expr.from_list [ Q.of_float 2.45; Q.of_float 2.45; Q.of_float 2.45 ];
      result;
      Lin_expr.from_list [ Q.of_float 2.5; Q.of_float 2.5; Q.of_float 2.5 ];
      Lin_expr.from_list [ Q.of_float 2.75; Q.of_float 2.75; Q.of_float 2.75 ];
      Lin_expr.from_list [ Q.of_float 2.45; Q.of_float 2.45; Q.of_float 2.45 ];
    ]
  and point = Point.from_list [ Q.of_int 2; Q.of_int 2 ] in
  match Lin_expr.find_supremum_term terms point with
  | Some term -> Lin_expr.equal term result
  | None -> false

(* PRINT *)
let%test "to_string" =
  let expr1 = Lin_expr.from_list [ Q.zero; Q.zero; Q.of_float (-2.5) ]
  and result1 = "-5/2"
  and expr2 = Lin_expr.from_list [ Q.minus_one; Q.zero; Q.of_float 2.5 ]
  and result2 = "-x₂ + 5/2"
  and expr3 =
    Lin_expr.from_list
      [ Q.of_float 2.5; Q.zero; Q.of_int 1; Q.of_int (-2); Q.of_int 3 ]
  and result3 = "5/2 x₄ + x₂ - 2x₁ + 3" in
  Lin_expr.to_string expr1 = result1
  && Lin_expr.to_string expr2 = result2
  && Lin_expr.to_string expr3 = result3
