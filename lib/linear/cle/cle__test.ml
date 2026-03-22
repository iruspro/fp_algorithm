open Cle

let ineq lhs rhs rel = Lin_ineq.construct lhs rhs rel
let lin coeffs = Lin_expr.from_list coeffs

(* CONSTRUCTORS *)
let%test "construct and getters roundtrip" =
  let c1 = ineq (Lin_expr.x 1) (Lin_expr.const Q.zero) Lt in
  let e = lin [ Q.one; Q.of_int 2 ] in
  let cle = construct [ c1 ] e in
  constraints cle = [ c1 ] && Lin_expr.equal (expr cle) e

let%test "construct empty constraints" =
  let e = lin [ Q.of_int 3 ] in
  let cle = construct [] e in
  constraints cle = [] && Lin_expr.equal (expr cle) e

(* FUNCTIONS *)
let%test "add_constraint" =
  let c1 = ineq (Lin_expr.x 1) (Lin_expr.const Q.zero) Lt in
  let c2 = ineq (Lin_expr.x 1) (Lin_expr.const Q.one) Le in
  let e = lin [ Q.one; Q.of_int 2 ] in
  let cle = construct [ c1 ] e in
  let cle' = add_constraint cle c2 in
  List.length (constraints cle') = 2
  && Lin_expr.equal (expr cle') e
  (* original unchanged *)
  && List.length (constraints cle) = 1

let%test "add_constraints" =
  let c1 = ineq (Lin_expr.x 1) (Lin_expr.const Q.zero) Lt in
  let c2 = ineq (Lin_expr.x 1) (Lin_expr.const Q.one) Le in
  let c3 = ineq (Lin_expr.x 2) (Lin_expr.const Q.zero) Ge in
  let cle = construct [ c1 ] (lin [ Q.one; Q.zero ]) in
  let cle' = add_constraints cle [ c2; c3 ] in
  List.length (constraints cle') = 3

let%test "add_constraints empty list" =
  let c1 = ineq (Lin_expr.x 1) (Lin_expr.const Q.zero) Lt in
  let cle = construct [ c1 ] (lin [ Q.of_int 5 ]) in
  let cle' = add_constraints cle [] in
  List.length (constraints cle') = 1

let%test "with_expr replaces expression" =
  let c1 = ineq (Lin_expr.x 1) (Lin_expr.const Q.zero) Lt in
  let e1 = lin [ Q.one; Q.of_int 2 ] in
  let e2 = lin [ Q.of_int 3 ] in
  let cle = construct [ c1 ] e1 in
  let cle' = with_expr cle e2 in
  Lin_expr.equal (expr cle') e2
  && constraints cle' = constraints cle
  (* original unchanged *)
  && Lin_expr.equal (expr cle) e1

let%test "mul_by scales expression" =
  let c1 = ineq (Lin_expr.x 1) (Lin_expr.const Q.zero) Lt in
  let e = lin [ Q.of_int 2; Q.of_int 4 ] in
  let cle = construct [ c1 ] e in
  let cle' = mul_by (Q.( // ) 1 2) cle in
  Lin_expr.equal (expr cle') (lin [ Q.one; Q.of_int 2 ])
  && constraints cle' = constraints cle

let%test "mul_by preserves constraints" =
  let c1 = ineq (Lin_expr.x 1) (Lin_expr.const Q.zero) Lt in
  let c2 = ineq (Lin_expr.x 1) (Lin_expr.const Q.one) Le in
  let cle = construct [ c1; c2 ] (lin [ Q.one; Q.zero ]) in
  let cle' = mul_by Q.zero cle in
  Lin_expr.equal (expr cle') (Lin_expr.const Q.zero)
  && List.length (constraints cle') = 2

(* PRINT *)
let%test "to_string empty constraints" =
  let cle = construct [] (Lin_expr.const Q.one) in
  to_string cle = "{} ⊢ 1"

let%test "to_string single constraint" =
  (* x₁ < 0 *)
  let c = ineq (Lin_expr.x 1) (Lin_expr.const Q.zero) Lt in
  let cle = construct [ c ] (Lin_expr.const (Q.of_int 5)) in
  to_string cle = "{\nx₁ < 0\n} ⊢ 5"

let%test "to_string two constraints" =
  let c1 = ineq (Lin_expr.x 1) (Lin_expr.const Q.zero) Lt in
  let c2 = ineq (Lin_expr.x 1) (Lin_expr.const Q.one) Le in
  let cle = construct [ c1; c2 ] (Lin_expr.x 2) in
  let s = to_string cle in
  (* two constraint lines separated by newline *)
  String.contains s '\n' && String.length s > 0
