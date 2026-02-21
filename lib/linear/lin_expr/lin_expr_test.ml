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
