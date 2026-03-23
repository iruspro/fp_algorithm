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

let%test "from_list single element" =
  let expr = from_list [ Q.of_int 5 ] in
  dim expr = 0 && Q.equal (leading_coeff expr) (Q.of_int 5)

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

let%test "const negative" =
  let expr = const Q.minus_one in
  equal expr (from_list [ Q.minus_one ]) && dim expr = 0

let%test "x 1" =
  let expr = x 1 and result = from_list [ Q.one; Q.zero ] in
  equal expr result && dim expr = 1

let%test "x 3" =
  let expr = x 3 and result = from_list [ Q.one; Q.zero; Q.zero; Q.zero ] in
  equal expr result && dim expr = 3

let%test "x 0 error" =
  try
    let _ = x 0 in
    false
  with Invalid_argument _ -> true

let%test "x negative error" =
  try
    let _ = x (-5) in
    false
  with Invalid_argument _ -> true

(* GETTERS *)
let%test "dim const" = dim (const Q.one) = 0

let%test "dim not const" =
  let expr = from_list [ Q.of_int 3; Q.zero; Q.of_int 7 ] in
  dim expr = 2

let%test "dim with leading zeros" =
  let expr = from_list [ Q.zero; Q.zero; Q.of_int 3; Q.zero; Q.of_int 7 ] in
  dim expr = 2

let%test "leading_coeff not const" =
  let expr = from_list [ Q.of_int 5; Q.zero; Q.zero; Q.one ] in
  Q.equal (Q.of_int 5) (leading_coeff expr)

let%test "leading_coeff with leading zeros" =
  let expr = from_list [ Q.zero; Q.zero; Q.of_int 5; Q.zero; Q.one ] in
  Q.equal (Q.of_int 5) (leading_coeff expr)

let%test "leading_coeff with only zeros" =
  let expr = from_list [ Q.zero; Q.zero; Q.zero ] in
  Q.equal Q.zero (leading_coeff expr)

let%test "coeff middle" =
  let expr =
    from_list [ Q.zero; Q.one; Q.of_int 7; Q.one; Q.one; Q.zero; Q.zero ]
  in
  Q.equal (Q.of_int 7) (coeff expr 4)

let%test "coeff i > dim" =
  let expr = const Q.one in
  Q.equal Q.zero (coeff expr 10)

let%test "coeff free coeff" =
  let expr = from_list [ Q.one; Q.zero; Q.of_int 42 ] in
  Q.equal (Q.of_int 42) (coeff expr 0)

let%test "coeff leading" =
  let expr = from_list [ Q.of_int 3; Q.one; Q.zero ] in
  Q.equal (Q.of_int 3) (coeff expr 2)

let%test "coeff negative index error" =
  let expr = const Q.zero in
  try
    let _ = coeff expr (-1) in
    false
  with Invalid_argument _ -> true

(* OPERATORS *)
let%test "equal same" =
  let expr1 = from_list [ Q.of_int 5; Q.of_int 2; Q.of_int 3 ]
  and expr2 = from_list [ Q.of_int 5; Q.of_int 2; Q.of_int 3 ] in
  equal expr1 expr2

let%test "equal reflexive" =
  let expr = from_list [ Q.of_int 5; Q.of_int 2; Q.of_int 3 ] in
  equal expr expr

let%test "equal different" =
  let expr1 = from_list [ Q.of_int 5; Q.of_int 2; Q.of_int 3 ]
  and expr2 = from_list [ Q.of_int 5; Q.of_int 2; Q.of_int 4 ] in
  not (equal expr1 expr2)

let%test "equal different dims" =
  let expr1 = from_list [ Q.one; Q.zero ]
  and expr2 = from_list [ Q.one; Q.one; Q.zero ] in
  not (equal expr1 expr2)

let%test "equal with leading zeros" =
  let expr1 = from_list [ Q.zero; Q.of_int 5; Q.of_int 2; Q.of_int 3 ]
  and expr2 =
    from_list [ Q.zero; Q.zero; Q.of_int 5; Q.of_int 2; Q.of_int 3 ]
  in
  equal expr1 expr2

let%test "mul_by non zero" =
  (* 10 * (1/3 y + 2x + 3) = 10/3 y + 20x + 30 *)
  let expr =
    mul_by (Q.of_int 10) (from_list [ Q.( // ) 1 3; Q.of_int 2; Q.of_int 3 ])
  and result = from_list [ Q.( // ) 10 3; Q.of_int 20; Q.of_int 30 ] in
  equal expr result && dim result = dim expr

let%test "mul_by one" =
  let expr = from_list [ Q.of_int 3; Q.of_int 2; Q.one ] in
  equal (mul_by Q.one expr) expr

let%test "mul_by minus one" =
  (* -1 * (3y + 2x + 1) = -3y - 2x - 1 *)
  let expr = from_list [ Q.of_int 3; Q.of_int 2; Q.one ] in
  let result = from_list [ Q.of_int (-3); Q.of_int (-2); Q.minus_one ] in
  equal (mul_by Q.minus_one expr) result

let%test "mul_by zero" =
  let expr = from_list [ Q.of_int 5; Q.of_int 3; Q.of_int 7 ] in
  equal (mul_by Q.zero expr) (const Q.zero) && dim (mul_by Q.zero expr) = 0

let%test "add simple" =
  (* (1/4 y + x + 1) + (1/2 y + 2x + 3) = 3/4 y + 3x + 4 *)
  let expr1 = from_list [ Q.( // ) 1 4; Q.one; Q.one ]
  and expr2 = from_list [ Q.( // ) 1 2; Q.of_int 2; Q.of_int 3 ] in
  let expr = add expr1 expr2
  and com_expr = add expr2 expr1
  and result = from_list [ Q.( // ) 3 4; Q.of_int 3; Q.of_int 4 ] in
  equal expr result && equal com_expr result && dim expr = 2 && dim com_expr = 2

let%test "add different dimensions" =
  (* (x + 1) + (1/2 y - x + 3) = 1/2 y + 4 *)
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
  and result = from_list [ Q.of_int 2; Q.of_int (-2) ] in
  equal expr result && dim expr = 1

let%test "add with zero" =
  let expr = from_list [ Q.of_int 3; Q.one; Q.of_int 5 ] in
  equal (add expr (const Q.zero)) expr && equal (add (const Q.zero) expr) expr

let%test "sub simple" =
  (* (10y - 5x + 3) - (y - 7.5x + 4) = 9y + 2.5x - 1 *)
  let expr1 = from_list [ Q.of_int 10; Q.of_int (-5); Q.of_int 3 ]
  and expr2 = from_list [ Q.one; Q.of_float (-7.5); Q.of_int 4 ] in
  let expr = sub expr1 expr2
  and result = from_list [ Q.of_int 9; Q.of_float 2.5; Q.minus_one ] in
  equal expr result && dim expr = 2

let%test "sub self is zero" =
  let expr = from_list [ Q.of_int 3; Q.of_int 7; Q.of_int 5 ] in
  let result = sub expr expr in
  equal result (const Q.zero) && dim result = 0

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

let%test "substitute with identity" =
  (* 3y + 2x + 1 with y := y -> 3y + 2x + 1 *)
  let expr = from_list [ Q.of_int 3; Q.of_int 2; Q.one ] in
  equal (substitute expr 2 (x 2)) expr

let%test "substitute index 0 error" =
  let expr = const Q.one in
  try
    let _ = substitute expr 0 expr in
    false
  with Invalid_argument _ -> true

let%test "substitute negative index error" =
  let expr = const Q.one in
  try
    let _ = substitute expr (-3) expr in
    false
  with Invalid_argument _ -> true

let%test "substitute_many" =
  (* 3x₂ + x₁ + 1, substitute x₁ := 2, x₂ := 5 => 15 + 2 + 1 = 18 *)
  let expr = from_list [ Q.of_int 3; Q.one; Q.one ] in
  let expr' =
    substitute_many expr [| 1; 2 |] [| const (Q.of_int 2); const (Q.of_int 5) |]
  in
  equal expr' (const (Q.of_int 18))

let%test "substitute_many single" =
  let expr = from_list [ Q.one; Q.of_int 2 ] in
  let expr' = substitute_many expr [| 1 |] [| const (Q.of_int 3) |] in
  equal expr' (const (Q.of_int 5))

let%test "substitute_many empty" =
  let expr = from_list [ Q.one; Q.of_int 2 ] in
  let expr' = substitute_many expr [||] [||] in
  equal expr' expr

let%test "substitute_many length mismatch raises" =
  let expr = x 1 in
  try
    let _ = substitute_many expr [| 1; 2 |] [| const Q.zero |] in
    false
  with Invalid_argument _ -> true

(* FUNCTIONS *)
let%test "eval const" =
  let expr = const (Q.of_int 42)
  and point = Point.from_list [ Q.one; Q.of_int 2; Q.of_int 3 ] in
  Q.equal (eval expr point) (Q.of_int 42)

let%test "eval const at empty point" =
  let expr = const (Q.of_int 7) and point = Point.from_list [] in
  Q.equal (eval expr point) (Q.of_int 7)

let%test "eval not const" =
  (* y + 2x + 3 at (2, 21/2) = 21/2 + 4 + 3 = 35/2 *)
  let expr = from_list [ Q.of_int 1; Q.of_int 2; Q.of_int 3 ]
  and point = Point.from_list [ Q.of_int 2; Q.( // ) 21 2 ] in
  Q.equal (eval expr point) (Q.( // ) 35 2)

let%test "eval dim point > dim expr" =
  (* y + 2x + 3 at (2, 21/2, 3, 4) = 35/2 *)
  let expr = from_list [ Q.of_int 1; Q.of_int 2; Q.of_int 3 ]
  and point =
    Point.from_list [ Q.of_int 2; Q.( // ) 21 2; Q.of_int 3; Q.of_int 4 ]
  in
  Q.equal (eval expr point) (Q.( // ) 35 2)

let%test "eval dim point < dim expr error" =
  let expr = from_list [ Q.of_int 1; Q.of_int 2; Q.of_int 3 ]
  and point = Point.from_list [ Q.of_int 2 ] in
  try
    let _ = eval expr point in
    false
  with Invalid_argument _ -> true

(* PRINT *)
let%test "to_string const zero" = to_string (const Q.zero) = "0"

let%test "to_string const negative" =
  let expr = from_list [ Q.zero; Q.zero; Q.of_float (-2.5) ] in
  to_string expr = "-5/2"

let%test "to_string start negative" =
  let expr = from_list [ Q.minus_one; Q.zero; Q.of_float 2.5 ] in
  to_string expr = "-x₂ + 5/2"

let%test "to_string mixed" =
  let expr =
    from_list [ Q.of_float 2.5; Q.zero; Q.of_int 1; Q.of_int (-2); Q.of_int 3 ]
  in
  to_string expr = "5/2 x₄ + x₂ - 2x₁ + 3"

let%test "to_string single var" = to_string (x 1) = "x₁"

let%test "to_string negative const term" =
  let expr = from_list [ Q.one; Q.of_int (-3) ] in
  to_string expr = "x₁ - 3"
