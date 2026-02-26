open Lin_expr

(* CONSTRUCTORS *)
let%test "from_list with leading zeros" =
  let expr = from_list [ Q.zero; Q.zero; Q.one; Q.zero; Q.one ]
  and result = from_list [ Q.one; Q.zero; Q.one ] in
  equal expr result && dim expr = 2 && leading_coeff expr <> Q.zero

let%test "from_list only zeros" =
  let expr = from_list [ Q.zero; Q.zero; Q.zero; Q.zero ]
  and result = from_list [ Q.zero ] in
  equal expr result && dim expr = 0

let%test "from_list empty list error" =
  try
    let _ = from_list [] in
    false
  with Invalid_argument _ -> true

let%test "const zero" =
  let expr = const Q.zero and result = from_list [ Q.zero ] in
  equal expr result && dim expr = 0

let%test "const one" =
  let expr = const Q.one and result = from_list [ Q.one ] in
  equal expr result && dim expr = 0

let%test "const random" =
  let q = Q.of_float (Random.float 1024.) in
  let expr = const q and result = from_list [ q ] in
  equal expr result && dim expr = 0

let%test "x" =
  let expr = x 3 and result = from_list [ Q.one; Q.zero; Q.zero; Q.zero ] in
  equal expr result && dim expr = 3

let%test "x invalid index error" =
  try
    let _ = x (-Random.int 1024) in
    false
  with Invalid_argument _ -> true

(* GETTERS *)
let%test "dim const" =
  let expr = const (Q.of_float (Random.float 1024.)) in
  dim expr = 0

let%test "dim not const" =
  let expr =
    from_list
      [
        Q.of_float (Random.float 1024. +. 1.);
        Q.zero;
        Q.of_float (Random.float 1024.);
      ]
  in
  dim expr = 2

let%test "dim with leading zeros" =
  let expr =
    from_list
      [
        Q.zero;
        Q.zero;
        Q.of_float (Random.float 1024. +. 1.);
        Q.zero;
        Q.of_float (Random.float 1024.);
      ]
  in
  dim expr = 2

let%test "leading coeff not const" =
  let q = Q.of_float (Random.float 1024. +. 1.) in
  let expr = from_list [ q; Q.zero; Q.zero; Q.one ] in
  Q.equal q (leading_coeff expr) && Q.equal Q.zero (leading_coeff expr)

let%test "leading coeff with leading zeros" =
  let q = Q.of_float (Random.float 1024. +. 1.) in
  let expr = from_list [ Q.zero; Q.zero; q; Q.zero; Q.zero; Q.one ] in
  Q.equal q (leading_coeff expr) && not (Q.equal Q.zero (leading_coeff expr))

let%test "leading_coeff with only zeros" =
  let expr = from_list [ Q.zero; Q.zero; Q.zero; Q.zero; Q.zero; Q.zero ] in
  Q.equal Q.zero (leading_coeff expr)

let%test "coeff i < dim expr" =
  let q = Q.of_float (Random.float 1024. +. 1.) in
  let expr = from_list [ Q.zero; Q.one; q; Q.one; Q.one; Q.zero; Q.zero ] in
  Q.equal q (coeff expr 4)

let%test "coeff i > dim" =
  let expr = const Q.one in
  Q.equal Q.zero (coeff expr (Random.int 1024 + 1))

let%test "coeff free coeff" =
  let q = Q.of_float (Random.float 1024. +. 1.) in
  let expr = from_list [ Q.zero; Q.one; Q.zero; Q.one; Q.one; Q.zero; q ] in
  Q.equal q (coeff expr 0)

let%test "coeff invalid index error" =
  let expr = const Q.zero in
  try
    let _ = coeff expr (-Random.int 1024 + 1) in
    false
  with Invalid_argument _ -> true

(* OPERATORS *)
let%test "equal" =
  let q = Q.of_float (Random.float 1024. +. 1.) in
  let expr1 = from_list [ q; Q.of_int 2; Q.of_int 3 ]
  and expr2 = from_list [ q; Q.of_int 2; Q.of_int 3 ]
  and expr3 = from_list [ q; Q.of_int 2; Q.of_int 4 ] in
  equal expr1 expr2 && not (equal expr1 expr3)

let%test "equal with leading zeros" =
  let q = Q.of_float (Random.float 1024. +. 1.) in
  let expr1 = from_list [ Q.zero; q; Q.of_int 2; Q.of_int 3 ]
  and expr2 = from_list [ Q.zero; Q.zero; q; Q.of_int 2; Q.of_int 3 ]
  and expr3 = from_list [ q; Q.of_int 2; Q.of_int 4 ] in
  equal expr1 expr2 && not (equal expr1 expr3)

let%test "mul_by non zero" =
  (* 10 * (1/3 y + 2x + 3) = 10/3 y + 20x + 30 *)
  let expr =
    mul_by (Q.of_int 10) (from_list [ Q.( // ) 1 3; Q.of_int 2; Q.of_int 3 ])
  and result = from_list [ Q.( // ) 10 3; Q.of_int 20; Q.of_int 30 ] in
  equal expr result && dim result = dim expr

let%test "mul by zero" =
  let expr =
    mul_by Q.zero
      (from_list
         [
           Q.of_float (Random.float 1024. +. 1.);
           Q.of_float (Random.float 1024. +. 1.);
           Q.of_float (Random.float 1024. +. 1.);
           Q.of_float (Random.float 1024. +. 1.);
         ])
  and result = const Q.zero in
  equal expr result && dim result = 0

let%test "add simple" =
  (* (1/4 y + x + 1) + (1/2 y + 2x + 3) = 3/4 y + 3x + 4 *)
  let expr1 = from_list [ Q.( // ) 1 4; Q.one; Q.one ]
  and expr2 = from_list [ Q.( // ) 1 2; Q.of_int 2; Q.of_int 3 ] in
  let expr = add expr1 expr2
  and com_expr = add expr2 expr1
  and result = from_list [ Q.( // ) 3 4; Q.of_int 3; Q.of_int 4 ] in
  equal expr result && equal com_expr result && dim expr = 2 && dim com_expr = 2

let%test "add different dimensions" =
  (* (x + 1) +  (1/2 y - x + 3) = 1/2 y + 4*)
  let expr1 = from_list [ Q.one; Q.one ]
  and expr2 = from_list [ Q.( // ) 1 2; Q.minus_one; Q.of_int 3 ] in
  let expr = add expr1 expr2
  and com_expr = add expr2 expr1
  and result = from_list [ Q.( // ) 1 2; Q.zero; Q.of_int 4 ] in
  equal expr result && equal com_expr result && dim expr = 2 && dim com_expr = 2

let%test "add with normalization" =
  (* (3z + 1/2 y + x - 1) + (-3z - 1/2 y + x - 1) = 2x - 2 *)
  let expr1 = from_list [ Q.of_int 3; Q.( // ) 1 2; Q.one; Q.minus_one ]
  and expr2 =
    from_list [ Q.of_int (-3); Q.( // ) (-1) 2; Q.one; Q.minus_one ]
  in
  let expr = add expr1 expr2
  and com_expr = add expr2 expr1
  and result = from_list [ Q.of_int 2; Q.of_int (-2) ] in
  equal expr result && equal com_expr result && dim expr = 1 && dim com_expr = 1

let%test "add with leading zeros" =
  (* (3z + 1/2 y + x - 1) + (-3z - 1/2 y + x - 1) = 2x - 2 *)
  let expr1 =
    from_list
      [
        Q.zero;
        Q.zero;
        Q.zero;
        Q.zero;
        Q.of_int 3;
        Q.( // ) 1 2;
        Q.one;
        Q.minus_one;
      ]
  and expr2 =
    from_list
      [ Q.zero; Q.zero; Q.of_int (-3); Q.( // ) (-1) 2; Q.one; Q.minus_one ]
  in
  let expr = add expr1 expr2
  and com_expr = add expr2 expr1
  and result = from_list [ Q.of_int 2; Q.of_int (-2) ] in
  equal expr result && equal com_expr result && dim expr = 1 && dim com_expr = 1

let%test "sub simple" =
  (* (10y - 5x + 3) - (y - 7.5x + 4) = 9y + 2.5x - 1 *)
  let expr1 = from_list [ Q.of_int 10; Q.of_int (-5); Q.of_int 3 ]
  and expr2 = from_list [ Q.one; Q.of_float (-7.5); Q.of_int 4 ] in
  let expr = sub expr1 expr2
  and result = from_list [ Q.of_int 9; Q.of_float 2.5; Q.minus_one ] in
  equal expr result && dim expr = 2

let%test "sub dim e1 > dim e2" =
  (* (10y - 5x + 3) - (7.5x + 4) = 10y + 2.5x - 1 *)
  let expr1 = from_list [ Q.of_int 10; Q.of_int (-5); Q.of_int 3 ]
  and expr2 = from_list [ Q.zero; Q.of_float (-7.5); Q.of_int 4 ] in
  let expr = sub expr1 expr2
  and result = from_list [ Q.of_int 10; Q.of_float 2.5; Q.minus_one ] in
  equal expr result && dim expr = 2

let%test "sub dim e1 < dim e2" =
  (* (-5x + 3) - (-10y + 7.5x + 4) = 10y - 12.5x - 1 *)
  let expr1 = from_list [ Q.of_int (-5); Q.of_int 3 ]
  and expr2 = from_list [ Q.of_int (-10); Q.of_float 7.5; Q.of_int 4 ] in
  let expr = sub expr1 expr2
  and result = from_list [ Q.of_int 10; Q.of_float (-12.5); Q.minus_one ] in
  equal expr result && dim expr = 2

let%test "sub with normalization" =
  (* (10y - 5x + 3) - (10y - 7.5x + 4) = 2.5x - 1 *)
  let expr1 = from_list [ Q.of_int 10; Q.of_int (-5); Q.of_int 3 ]
  and expr2 = from_list [ Q.of_int 10; Q.of_float (-7.5); Q.of_int 4 ] in
  let expr = sub expr1 expr2
  and result = from_list [ Q.of_float 2.5; Q.minus_one ] in
  equal expr result && dim expr = 1

let%test "sub with leading zeros" =
  (* (10y - 5x + 3) - (y - 7.5x + 4) = 9y + 2.5x - 1 *)
  let expr1 =
    from_list [ Q.zero; Q.zero; Q.of_int 10; Q.of_int (-5); Q.of_int 3 ]
  and expr2 =
    from_list [ Q.zero; Q.zero; Q.zero; Q.one; Q.of_float (-7.5); Q.of_int 4 ]
  in
  let expr = sub expr1 expr2
  and result = from_list [ Q.of_int 9; Q.of_float 2.5; Q.minus_one ] in
  equal expr result && dim expr = 2

let%test "substitute simple" =
  (* 5y + 2x + 1 with y := 1/2 y + 2x + 5 -> 5/2 y + 12x + 26 *)
  let expr1 = from_list [ Q.of_int 5; Q.of_int 2; Q.of_int 1 ]
  and expr2 = from_list [ Q.( // ) 1 2; Q.of_int 2; Q.of_int 5 ] in
  let expr = substitute expr1 2 expr2
  and result = from_list [ Q.( // ) 5 2; Q.of_int 12; Q.of_int 26 ] in
  equal expr result && dim expr = 2

let%test "substitute and reduce dim" =
  (* 5y + 2x + 1 with y := 2x + 5 -> 12x + 26 *)
  let expr1 = from_list [ Q.of_int 5; Q.of_int 2; Q.of_int 1 ]
  and expr2 = from_list [ Q.of_int 2; Q.of_int 5 ] in
  let expr = substitute expr1 2 expr2
  and result = from_list [ Q.of_int 12; Q.of_int 26 ] in
  equal expr result && dim expr = 1

let%test "substitute with coeff = 0" =
  (* 2x + 1 with y := 1/2 y + 2x + 5 -> 2x + 1 *)
  let expr1 = from_list [ Q.of_int 2; Q.of_int 1 ]
  and expr2 = from_list [ Q.( // ) 1 2; Q.of_int 2; Q.of_int 5 ] in
  let expr = substitute expr1 2 expr2
  and result = from_list [ Q.of_int 2; Q.of_int 1 ] in
  equal expr result && dim expr = 1

let%test "substitute middle var" =
  (* 5y + 2x + 1 with x := 1/2 y + 2x + 5 -> 6y + 4x + 11 *)
  let expr1 = from_list [ Q.of_int 5; Q.of_int 2; Q.of_int 1 ]
  and expr2 = from_list [ Q.( // ) 1 2; Q.of_int 2; Q.of_int 5 ] in
  let expr = substitute expr1 1 expr2
  and result = from_list [ Q.of_int 6; Q.of_int 4; Q.of_int 11 ] in
  equal expr result && dim expr = 2

let%test "substitute with leading zeros" =
  (* 5y + 2x + 1 with y := 1/2 y + 2x + 5 -> 5/2 y + 12x + 26 *)
  let expr1 = from_list [ Q.zero; Q.zero; Q.of_int 5; Q.of_int 2; Q.of_int 1 ]
  and expr2 =
    from_list [ Q.zero; Q.zero; Q.zero; Q.( // ) 1 2; Q.of_int 2; Q.of_int 5 ]
  in
  let expr = substitute expr1 2 expr2
  and result = from_list [ Q.( // ) 5 2; Q.of_int 12; Q.of_int 26 ] in
  equal expr result && dim expr = 2

let%test "substitute invalid index" =
  let expr = const Q.one in
  try
    let _ = substitute expr (-Random.int 1024) expr in
    false
  with Invalid_argument _ -> true

(* FUNCTIONS *)
let%test "eval const" =
  (* random const at random point *)
  let q = Q.of_float (Random.float 1024.) in
  let expr = const q
  and point =
    Point.from_list
      (List.init (Random.int 1024) (fun _ -> Q.of_float (Random.float 1024.)))
  in
  eval expr point = q

let%test "eval not const" =
  (* y + 2x + 3 at (2; 10.5)*)
  let expr = from_list [ Q.of_int 1; Q.of_int 2; Q.of_int 3 ]
  and point = Point.from_list [ Q.of_int 2; Q.( // ) 21 2 ] in
  eval expr point = Q.( // ) 35 2

let%test "eval dim point > dim expr" =
  (* y + 2x + 3 at (2; 10.5; 3; 4)*)
  let expr = from_list [ Q.of_int 1; Q.of_int 2; Q.of_int 3 ]
  and point =
    Point.from_list [ Q.of_int 2; Q.( // ) 21 2; Q.of_int 3; Q.of_int 4 ]
  in
  eval expr point = Q.( // ) 35 2

let%test "eval with leading zeros" =
  (* y + 2x + 3 at (2; 10.5)*)
  let expr = from_list [ Q.zero; Q.zero; Q.of_int 1; Q.of_int 2; Q.of_int 3 ]
  and point = Point.from_list [ Q.of_int 2; Q.( // ) 21 2 ] in
  eval expr point = Q.( // ) 35 2

let%test "eval dim point < dim expr" =
  (* y + 2x + 3 at (2,)*)
  let expr = from_list [ Q.of_int 1; Q.of_int 2; Q.of_int 3 ]
  and point = Point.from_list [ Q.of_int 2 ] in
  try
    let _ = eval expr point in
    false
  with Invalid_argument _ -> true

(* PRINT *)
let%test "to_string const" =
  let expr = from_list [ Q.zero; Q.zero; Q.of_float (-2.5) ] in
  to_string expr = "-5/2"

let%test "to_string not const with zeros start -" =
  let expr = from_list [ Q.minus_one; Q.zero; Q.of_float 2.5 ] in
  to_string expr = "-x₂ + 5/2"

let%test "to_string not const with zeros start +" =
  let expr =
    from_list [ Q.of_float 2.5; Q.zero; Q.of_int 1; Q.of_int (-2); Q.of_int 3 ]
  in
  to_string expr = "5/2 x₄ + x₂ - 2x₁ + 3"
