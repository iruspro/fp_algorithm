open Lin_ineq

let expr = Lin_expr.from_list
let cnst q = Lin_expr.const q
let x = Lin_expr.x

(* CONSTRUCTORS *)
let%test "construct constant inequality" =
  (* 3 < 5  =>  0 < 2 *)
  let ineq = construct (cnst (Q.of_int 3)) (cnst (Q.of_int 5)) Lt in
  dim ineq = 0
  && Lin_expr.equal (lhs ineq) (cnst Q.zero)
  && Lin_expr.equal (rhs ineq) (cnst (Q.of_int 2))
  && rel ineq = Lt

let%test "construct normalizes to x_n on lhs" =
  (* x₁ + 1 < 2x₁ + 3  =>  rhs - lhs = x₁ + 2  =>  x₁ < 2 *)
  (* Wait: rhs - lhs = (2x₁ + 3) - (x₁ + 1) = x₁ + 2, leading_coeff = 1 (positive)
     x = x₁, rhs' = (x₁ + 2) - 1·x₁ = 2, rhs'' = mul_by(-1/1) 2 = -2...
     Hmm let me re-derive. *)
  (* Actually: rhs = sub (2x₁+3) (x₁+1) = x₁+2, dim=1, lc=1
     x = x₁
     rhs' = sub (x₁+2) (mul_by 1 x₁) = sub (x₁+2) x₁ = 2
     rhs'' = mul_by (neg (inv 1)) 2 = mul_by (-1) 2 = -2
     rel: geq (neg 1) 0 = geq (-1) 0 = false, so rev_rel Lt = Gt
     Result: x₁ Gt -2, i.e. x₁ > -2 *)
  let ineq =
    construct (expr [ Q.one; Q.one ]) (expr [ Q.of_int 2; Q.of_int 3 ]) Lt
  in
  dim ineq = 1
  && Lin_expr.equal (lhs ineq) (x 1)
  && Lin_expr.equal (rhs ineq) (cnst (Q.of_int (-2)))
  && rel ineq = Gt

let%test "construct positive leading coeff preserves rel" =
  (* 0 < x₁  =>  rhs - lhs = x₁, lc = 1 (positive)
     rhs' = x₁ - 1·x₁ = 0
     rhs'' = mul_by (-1) 0 = 0
     rel: geq (neg 1) 0 = false => rev_rel Lt = Gt
     Actually wait: neg leading_coeff = neg 1 = -1, geq (-1) 0 = false => rev_rel
     Hmm, that seems off. Let me re-check the code. *)
  (* Code line 29: rel = if Q.geq (Q.neg leading_coeff) Q.zero then rel else rev_rel rel
     leading_coeff of (rhs - lhs). If rhs-lhs = x₁, lc = 1.
     neg lc = -1. geq (-1) 0 = false => rev_rel Lt = Gt.
     So: 0 < x₁ => x₁ > 0. That's correct! *)
  let ineq = construct (cnst Q.zero) (x 1) Lt in
  dim ineq = 1
  && Lin_expr.equal (lhs ineq) (x 1)
  && rel ineq = Gt
  && Lin_expr.equal (rhs ineq) (cnst Q.zero)

let%test "construct negative leading coeff reverses rel" =
  (* x₁ < 0  =>  rhs - lhs = 0 - x₁ = -x₁, lc = -1
     neg lc = 1, geq 1 0 = true => rel stays Lt
     x = x₁, rhs' = (-x₁) - (-1)·x₁ = -x₁ + x₁ = 0
     rhs'' = mul_by (neg (inv (-1))) 0 = mul_by 1 · 0 = 0
     Result: x₁ < 0 *)
  let ineq = construct (x 1) (cnst Q.zero) Lt in
  dim ineq = 1
  && Lin_expr.equal (lhs ineq) (x 1)
  && rel ineq = Lt
  && Lin_expr.equal (rhs ineq) (cnst Q.zero)

let%test "construct two variables" =
  (* 2x₂ + x₁ ≤ x₂ + 3x₁ + 1
     rhs - lhs = -x₂ + 2x₁ + 1, dim=2, lc = -1
     neg lc = 1, geq 1 0 = true => rel stays Le
     x = x₂, rhs' = (-x₂ + 2x₁ + 1) - (-1)·x₂ = 2x₁ + 1
     rhs'' = mul_by (neg (inv (-1))) (2x₁ + 1) = mul_by 1 (2x₁+1) = 2x₁+1
     Result: x₂ ≤ 2x₁ + 1 *)
  let ineq =
    construct
      (expr [ Q.of_int 2; Q.one; Q.zero ])
      (expr [ Q.one; Q.of_int 3; Q.one ])
      Le
  in
  dim ineq = 2
  && Lin_expr.equal (lhs ineq) (x 2)
  && Lin_expr.equal (rhs ineq) (expr [ Q.of_int 2; Q.one ])
  && rel ineq = Le

let%test "construct same lhs and rhs" =
  (* x₁ + 1 < x₁ + 1  =>  rhs - lhs = 0, dim = 0 => 0 < 0 *)
  let e = expr [ Q.one; Q.one ] in
  let ineq = construct e e Lt in
  dim ineq = 0
  && Lin_expr.equal (lhs ineq) (cnst Q.zero)
  && Lin_expr.equal (rhs ineq) (cnst Q.zero)

(* GETTERS *)
let%test "dim" =
  let ineq0 = construct (cnst Q.one) (cnst (Q.of_int 2)) Lt in
  let ineq1 = construct (x 1) (cnst Q.zero) Lt in
  let ineq2 = construct (x 2) (x 1) Le in
  dim ineq0 = 0 && dim ineq1 = 1 && dim ineq2 = 2

(* OPERATORS *)
let%test "negate Lt becomes Ge" =
  let ineq = construct (x 1) (cnst Q.zero) Lt in
  rel (negate ineq) = Ge

let%test "negate Le becomes Gt" =
  let ineq = construct (x 1) (cnst Q.zero) Le in
  rel (negate ineq) = Gt

let%test "negate Gt becomes Le" =
  let ineq = construct (x 1) (cnst Q.zero) Gt in
  rel (negate ineq) = Le

let%test "negate Ge becomes Lt" =
  let ineq = construct (x 1) (cnst Q.zero) Ge in
  rel (negate ineq) = Lt

let%test "negate preserves dim" =
  let ineq = construct (x 2) (x 1) Lt in
  dim (negate ineq) = dim ineq

let%test "negate involution" =
  let ineq = construct (x 1) (cnst (Q.of_int 3)) Le in
  let ineq'' = negate (negate ineq) in
  Lin_expr.equal (lhs ineq'') (lhs ineq)
  && Lin_expr.equal (rhs ineq'') (rhs ineq)
  && rel ineq'' = rel ineq

let%test "substitute i > dim is identity" =
  let ineq = construct (x 1) (cnst Q.zero) Lt in
  let ineq' = substitute ineq 2 (cnst Q.one) in
  Lin_expr.equal (lhs ineq') (lhs ineq)
  && Lin_expr.equal (rhs ineq') (rhs ineq)
  && rel ineq' = rel ineq

let%test "substitute i < 1 raises" =
  let ineq = construct (x 1) (cnst Q.zero) Lt in
  try
    let _ = substitute ineq 0 (cnst Q.one) in
    false
  with Invalid_argument _ -> true

let%test "substitute rhs variable" =
  (* x₂ ≤ 3x₁ + 1, substitute x₁ := 2 => x₂ ≤ 7 *)
  let ineq = construct (x 2) (expr [ Q.of_int 3; Q.one ]) Le in
  let ineq' = substitute ineq 1 (cnst (Q.of_int 2)) in
  dim ineq' = 2 && Lin_expr.equal (rhs ineq') (cnst (Q.of_int 7))

let%test "substitute lhs variable" =
  (* x₁ < 3, substitute x₁ := x₂ + 1
     construct (x₂ + 1) (const 3) Lt
     rhs - lhs = 3 - x₂ - 1 = -x₂ + 2, dim=2, lc=-1
     neg lc = 1, geq 1 0 = true => rel stays Lt
     rhs' = (-x₂+2) - (-1)·x₂ = 2
     rhs'' = mul_by 1 · 2 = 2
     Result: x₂ < 2 *)
  let ineq = construct (x 1) (cnst (Q.of_int 3)) Lt in
  let ineq' = substitute ineq 1 (expr [ Q.one; Q.zero; Q.one ]) in
  dim ineq' = 2
  && Lin_expr.equal (lhs ineq') (x 2)
  && Lin_expr.equal (rhs ineq') (cnst (Q.of_int 2))
  && rel ineq' = Lt

(* FUNCTIONS *)
let%test "is_satisfied Lt true" =
  (* x₁ < 5 at x₁ = 3 *)
  let ineq = construct (x 1) (cnst (Q.of_int 5)) Lt in
  is_satisfied ineq (Point.from_list [ Q.of_int 3 ])

let%test "is_satisfied Lt false at boundary" =
  (* x₁ < 5 at x₁ = 5 *)
  let ineq = construct (x 1) (cnst (Q.of_int 5)) Lt in
  not (is_satisfied ineq (Point.from_list [ Q.of_int 5 ]))

let%test "is_satisfied Le true at boundary" =
  (* x₁ ≤ 5 at x₁ = 5 *)
  let ineq = construct (x 1) (cnst (Q.of_int 5)) Le in
  is_satisfied ineq (Point.from_list [ Q.of_int 5 ])

let%test "is_satisfied Gt" =
  (* x₁ > 0 at x₁ = 1 *)
  let ineq = construct (x 1) (cnst Q.zero) Gt in
  is_satisfied ineq (Point.from_list [ Q.one ])
  && not (is_satisfied ineq (Point.from_list [ Q.zero ]))

let%test "is_satisfied constant true" =
  (* 0 ≤ 5 *)
  let ineq = construct (cnst Q.zero) (cnst (Q.of_int 5)) Le in
  is_satisfied ineq (Point.from_list [ Q.zero ])

let%test "is_satisfied constant false" =
  (* 0 > 5 *)
  let ineq = construct (cnst Q.zero) (cnst (Q.of_int 5)) Gt in
  not (is_satisfied ineq (Point.from_list [ Q.zero ]))

let%test "is_satisfied two variables" =
  (* x₂ ≤ x₁ + 1 at (2, 3) => 3 ≤ 3 => true *)
  let ineq = construct (x 2) (expr [ Q.one; Q.one ]) Le in
  is_satisfied ineq (Point.from_list [ Q.of_int 2; Q.of_int 3 ])

(* PRINT *)
let%test "to_string simple" =
  (* x₁ < 0 *)
  let ineq = construct (x 1) (cnst Q.zero) Lt in
  to_string ineq = "x₁ < 0"

let%test "to_string constant" =
  let ineq = construct (cnst Q.one) (cnst (Q.of_int 3)) Lt in
  to_string ineq = "0 < 2"

let%test "to_string Gt" =
  let ineq = construct (cnst Q.zero) (x 1) Lt in
  (* normalizes to x₁ > 0 *)
  to_string ineq = "x₁ > 0"
