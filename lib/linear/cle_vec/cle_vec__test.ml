open Cle_vec

let ineq lhs rhs rel = Lin_ineq.construct lhs rhs rel
let lin coeffs = Lin_expr.from_list coeffs

(* CONSTRUCTORS *)
let%test "construct and getters roundtrip" =
  let c1 = ineq (Lin_expr.x 1) (Lin_expr.const Q.zero) Lt in
  let e1 = lin [ Q.one; Q.of_int 2 ] and e2 = lin [ Q.of_int 3 ] in
  let cle = construct [ c1 ] [| e1; e2 |] in
  constraints cle = [ c1 ]
  && Lin_expr.equal (expr cle 0) e1
  && Lin_expr.equal (expr cle 1) e2
  && Array.length (exprs cle) = 2

let%test "construct empty constraints" =
  let cle = construct [] [| lin [ Q.one ] |] in
  constraints cle = [] && Array.length (exprs cle) = 1

(* FUNCTIONS *)
let%test "add_constraint" =
  let c1 = ineq (Lin_expr.x 1) (Lin_expr.const Q.zero) Lt in
  let c2 = ineq (Lin_expr.x 1) (Lin_expr.const Q.one) Le in
  let cle = construct [ c1 ] [| lin [ Q.one ] |] in
  let cle' = add_constraint cle c2 in
  List.length (constraints cle') = 2 && List.length (constraints cle) = 1

let%test "add_constraints" =
  let c1 = ineq (Lin_expr.x 1) (Lin_expr.const Q.zero) Lt in
  let c2 = ineq (Lin_expr.x 1) (Lin_expr.const Q.one) Le in
  let c3 = ineq (Lin_expr.x 2) (Lin_expr.const Q.zero) Ge in
  let cle = construct [ c1 ] [| lin [ Q.one ] |] in
  let cle' = add_constraints cle [ c2; c3 ] in
  List.length (constraints cle') = 3

let%test "with_expr replaces k-th expression" =
  let e1 = lin [ Q.one ] and e2 = lin [ Q.of_int 2 ] in
  let e3 = lin [ Q.of_int 99 ] in
  let cle = construct [] [| e1; e2 |] in
  let cle' = with_expr cle e3 1 in
  Lin_expr.equal (expr cle' 0) e1 && Lin_expr.equal (expr cle' 1) e3

let%test "with_expr does not mutate original" =
  let e1 = lin [ Q.one ] and e2 = lin [ Q.of_int 2 ] in
  let e3 = lin [ Q.of_int 99 ] in
  let cle = construct [] [| e1; e2 |] in
  let _ = with_expr cle e3 1 in
  Lin_expr.equal (expr cle 1) e2

let%test "with_exprs replaces all" =
  let c1 = ineq (Lin_expr.x 1) (Lin_expr.const Q.zero) Lt in
  let cle = construct [ c1 ] (vec [| lin [ Q.one ]; lin [ Q.of_int 2 ] |]) in
  let new_es = vec [| Lin_expr.x 3; Lin_expr.const Q.zero |] in
  let cle' = with_exprs cle new_es in
  Lin_expr.equal (expr cle' 1) (Lin_expr.x 3)
  && Lin_expr.equal (expr cle' 2) (Lin_expr.const Q.zero)
  && constraints cle' = [ c1 ]

let%test "with_exprs preserves original" =
  let cle = construct [] (vec [| lin [ Q.one ] |]) in
  let _ = with_exprs cle (vec [| Lin_expr.x 5 |]) in
  Lin_expr.equal (expr cle 1) (lin [ Q.one ])

let%test "substitute_from affects components from k" =
  (* (x₁ + x₂, 3x₂, x₂ + 1), substitute x₂ := 5 from component 2 *)
  let cle =
    construct []
      (vec
         [|
           lin [ Q.one; Q.one; Q.zero ];
           lin [ Q.of_int 3; Q.zero; Q.zero ];
           lin [ Q.one; Q.zero; Q.one ];
         |])
  in
  let cle' = substitute_from cle 2 2 (Lin_expr.const (Q.of_int 5)) in
  (* component 1 unchanged *)
  Lin_expr.equal (expr cle' 1) (lin [ Q.one; Q.one; Q.zero ])
  (* component 2: 3*5 = 15 *)
  && Lin_expr.equal (expr cle' 2) (Lin_expr.const (Q.of_int 15))
  (* component 3: 5 + 1 = 6 *)
  && Lin_expr.equal (expr cle' 3) (Lin_expr.const (Q.of_int 6))

let%test "substitute_from all components" =
  (* (x₁, x₁ + 1), substitute x₁ := 0 from component 1 *)
  let cle = construct [] (vec [| Lin_expr.x 1; lin [ Q.one; Q.one ] |]) in
  let cle' = substitute_from cle 1 1 (Lin_expr.const Q.zero) in
  Lin_expr.equal (expr cle' 1) (Lin_expr.const Q.zero)
  && Lin_expr.equal (expr cle' 2) (Lin_expr.const Q.one)

let%test "substitute_from i < 1 raises" =
  let cle = construct [] (vec [| Lin_expr.x 1 |]) in
  try
    let _ = substitute_from cle 1 0 (Lin_expr.const Q.zero) in
    false
  with Invalid_argument _ -> true

let%test "substitute_constraints" =
  (* constraint: x₁ < x₂, substitute x₂ := 3 => x₁ < 3 *)
  let c = ineq (Lin_expr.x 1) (Lin_expr.x 2) Lt in
  let cle = construct [ c ] (vec [| Lin_expr.x 1; Lin_expr.x 2 |]) in
  let cle' = substitute_constraints cle 2 (Lin_expr.const (Q.of_int 3)) in
  (* expressions unchanged *)
  Lin_expr.equal (expr cle' 1) (Lin_expr.x 1)
  && Lin_expr.equal (expr cle' 2) (Lin_expr.x 2)
  (* constraint updated *)
  && List.length (constraints cle') = 1

let%test "substitute_from preserves constraints" =
  let c1 = ineq (Lin_expr.x 1) (Lin_expr.const Q.zero) Lt in
  let cle = construct [ c1 ] (vec [| Lin_expr.x 1 |]) in
  let cle' = substitute_from cle 1 1 (Lin_expr.const Q.zero) in
  constraints cle' = [ c1 ]

(* PRINT *)
let%test "to_string empty constraints" =
  let cle = construct [] [| Lin_expr.const Q.one; Lin_expr.const Q.zero |] in
  to_string cle = "{} ⊢ (1, 0)"

let%test "to_string single expr" =
  let cle = construct [] [| Lin_expr.x 1 |] in
  to_string cle = "{} ⊢ (x₁)"

let%test "to_string with constraint" =
  let c = ineq (Lin_expr.x 1) (Lin_expr.const Q.zero) Lt in
  let cle = construct [ c ] [| Lin_expr.const (Q.of_int 5) |] in
  to_string cle = "{\nx₁ < 0\n} ⊢ (5)"
