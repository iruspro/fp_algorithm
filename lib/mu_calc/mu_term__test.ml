open Mu_term
open Linear

(* CONSTRUCTORS *)
let%test "var valid" = to_string (var 1) = "x₁"

let%test "var invalid" =
  try
    let _ = var 0 in
    false
  with Invalid_argument _ -> true

let%test "const zero" = to_string (const Q.zero) = "0"
let%test "const one" = to_string (const Q.one) = "1"
let%test "const fraction" = to_string (const (Q.( // ) 3 4)) = "3/4"

let%test "const out of range" =
  try
    let _ = const (Q.of_int 2) in
    false
  with Invalid_argument _ -> true

let%test "const negative" =
  try
    let _ = const Q.minus_one in
    false
  with Invalid_argument _ -> true

let%test "scalar_mult" =
  to_string (scalar_mult (Q.( // ) 1 2) (var 1)) = "1/2 x₁"

let%test "scalar_mult out of range" =
  try
    let _ = scalar_mult (Q.of_int 2) (var 1) in
    false
  with Invalid_argument _ -> true

let%test "weak_disj" = to_string (weak_disj (var 1) (var 2)) = "x₁ ⊔ x₂"
let%test "weak_conj" = to_string (weak_conj (var 1) (var 2)) = "x₁ ⊓ x₂"

let%test "strong_disj" = to_string (strong_disj (var 1) (var 2)) = "x₁ ⊕ x₂"

let%test "strong_conj" = to_string (strong_conj (var 1) (var 2)) = "x₁ ⊙ x₂"

let%test "mu valid" = to_string (mu 1 (var 1)) = "μx₁.x₁"
let%test "nu valid" = to_string (nu 1 (var 1)) = "νx₁.x₁"

let%test "mu invalid" =
  try
    let _ = mu 0 (var 1) in
    false
  with Invalid_argument _ -> true

let%test "nu invalid" =
  try
    let _ = nu 0 (var 1) in
    false
  with Invalid_argument _ -> true

(* SIMPLIFY *)
let%test "simplify scalar_mult zero" =
  to_string (simplify (scalar_mult Q.zero (var 1))) = "0"

let%test "simplify scalar_mult one" =
  to_string (simplify (scalar_mult Q.one (var 1))) = "x₁"

let%test "simplify scalar_mult of zero term" =
  to_string (simplify (scalar_mult (Q.( // ) 1 2) (const Q.zero))) = "0"

let%test "simplify weak_disj with one" =
  to_string (simplify (weak_disj (var 1) (const Q.one))) = "1"

let%test "simplify weak_disj with one lhs" =
  to_string (simplify (weak_disj (const Q.one) (var 1))) = "1"

let%test "simplify weak_conj with zero" =
  to_string (simplify (weak_conj (var 1) (const Q.zero))) = "0"

let%test "simplify weak_conj with zero lhs" =
  to_string (simplify (weak_conj (const Q.zero) (var 1))) = "0"

let%test "simplify strong_disj with zero" =
  to_string (simplify (strong_disj (var 1) (const Q.zero))) = "x₁"

let%test "simplify strong_disj with zero lhs" =
  to_string (simplify (strong_disj (const Q.zero) (var 1))) = "x₁"

let%test "simplify strong_conj with one" =
  to_string (simplify (strong_conj (var 1) (const Q.one))) = "x₁"

let%test "simplify strong_conj with one lhs" =
  to_string (simplify (strong_conj (const Q.one) (var 1))) = "x₁"

let%test "simplify nested" =
  (* (x₁ ⊕ 0) ⊙ 1 → x₁ *)
  let term = strong_conj (strong_disj (var 1) (const Q.zero)) (const Q.one) in
  to_string (simplify term) = "x₁"

let%test "simplify no-op" =
  let term = strong_disj (var 1) (var 2) in
  to_string (simplify term) = "x₁ ⊕ x₂"

let%test "simplify const zero" = to_string (simplify (const Q.zero)) = "0"
let%test "simplify const one" = to_string (simplify (const Q.one)) = "1"

(* EVAL *)
let mu_term =
  (* μx₂.(5/8 ⊕ 3/8 x₁) ⊙ (1/2 ⊔ (3/8 ⊕ 1/2 x₂)) *)
  simplify
    (mu 2
       (strong_conj
          (strong_disj
             (const (Q.( // ) 5 8))
             (scalar_mult (Q.( // ) 3 8) (var 1)))
          (weak_disj
             (const (Q.( // ) 1 2))
             (strong_disj
                (const (Q.( // ) 3 8))
                (scalar_mult (Q.( // ) 1 2) (var 2))))))

let%test "eval" =
  let point = Point.from_list [ Q.one ] in
  let cle = eval 1 mu_term point in
  let constraints = Cond_lin_expr.constraints cle
  and expr = Cond_lin_expr.expr cle in
  let fp = Lin_expr.eval expr point in
  let constraints_ok =
    List.for_all (fun ineq -> Lin_ineq.is_satisfied ineq point) constraints
  in
  Q.equal fp (Q.( // ) 3 4) && constraints_ok

let%test "eval dim mismatch" =
  let point = Point.from_list [ Q.one; Q.zero ] in
  try
    let _ = eval 1 (var 1) point in
    false
  with Invalid_argument _ -> true

(* PRINT *)
let%test "to_string compound" =
  to_string mu_term = "μx₂.(5/8 ⊕ 3/8 x₁) ⊙ (1/2 ⊔ (3/8 ⊕ 1/2 x₂))"
