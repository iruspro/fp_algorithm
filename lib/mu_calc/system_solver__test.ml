open Core
open Linear

(** Construct a [Local_alg_vec.t] from an array of [Mu_term.t] bodies,
    evaluating each term at dimension [dim]. *)
let make_system dim terms : Local_alg_vec.t =
 fun point ->
  let cles = Array.map (fun t -> Mu_term.eval dim t point) terms in
  let all_constraints =
    Array.fold_left (fun acc cle -> Cle.constraints cle @ acc) [] cles
  in
  let exprs = Lin_expr_vec.from_array (Array.map Cle.expr cles) in
  Cle_vec.construct all_constraints exprs

let constraints_ok result point =
  List.for_all
    (fun ineq -> Lin_ineq.is_satisfied ineq point)
    (Cle_vec.constraints result)

(* ---- codim=2, independent ----
   f₁ = 1/4 x₁ ⊕ 1/2 x₂   (body for x₂)
   f₂ = 1/4 ⊕ 1/2 x₃       (body for x₃)
   Expected: x₂ = 1/2 x₁, x₃ = 1/2 *)

let system_2_indep =
  let t1 =
    Mu_term.(
      strong_disj
        (scalar_mult (Q.( // ) 1 4) (var 1))
        (scalar_mult (Q.( // ) 1 2) (var 2)))
  in
  let t2 =
    Mu_term.(
      strong_disj (const (Q.( // ) 1 4)) (scalar_mult (Q.( // ) 1 2) (var 3)))
  in
  make_system 3 [| t1; t2 |]

let%test "lfp_system codim=2 independent, x₁=1/2" =
  let point = Point.from_list [ Q.( // ) 1 2 ] in
  let result = System_solver.lfp_system 3 2 system_2_indep point in
  Cle_vec.print result;
  let e1 = Cle_vec.expr result 1 and e2 = Cle_vec.expr result 2 in
  Q.equal (Lin_expr.eval e1 point) (Q.( // ) 1 4)
  && Q.equal (Lin_expr.eval e2 point) (Q.( // ) 1 2)
  && constraints_ok result point

let%test "lfp_system codim=2 independent, x₁=1" =
  let point = Point.from_list [ Q.one ] in
  let result = System_solver.lfp_system 3 2 system_2_indep point in
  (* Cle_vec.print result; *)
  let e1 = Cle_vec.expr result 1 and e2 = Cle_vec.expr result 2 in
  Q.equal (Lin_expr.eval e1 point) (Q.( // ) 1 2)
  && Q.equal (Lin_expr.eval e2 point) (Q.( // ) 1 2)
  (* && constraints_ok result point *)

(* ---- codim=2, coupled ----
   f₁ = 1/4 x₁ ⊕ 1/4 x₃   (body for x₂, depends on x₃)
   f₂ = 1/4 ⊕ 1/4 x₂       (body for x₃, depends on x₂)
   Expected at x₁=1/2: x₂ = 1/5, x₃ = 3/10 *)

let%test "lfp_system codim=2 coupled" =
  let t1 =
    Mu_term.(
      strong_disj
        (scalar_mult (Q.( // ) 1 4) (var 1))
        (scalar_mult (Q.( // ) 1 4) (var 3)))
  in
  let t2 =
    Mu_term.(
      strong_disj (const (Q.( // ) 1 4)) (scalar_mult (Q.( // ) 1 4) (var 2)))
  in
  let f = make_system 3 [| t1; t2 |] in
  let point = Point.from_list [ Q.( // ) 1 2 ] in
  let result = System_solver.lfp_system 3 2 f point in
  (* Cle_vec.print result; *)
  let e1 = Cle_vec.expr result 1 and e2 = Cle_vec.expr result 2 in
  Q.equal (Lin_expr.eval e1 point) (Q.( // ) 1 5)
  && Q.equal (Lin_expr.eval e2 point) (Q.( // ) 3 10)
  (* && constraints_ok result point *)

(* ---- codim=3, chain ----
   Expected at x₁=1/2: x₂ = 1/4, x₃ = 1/8, x₄ = 1/2 *)

let%test "lfp_system codim=3 chain" =
  let t1 =
    Mu_term.(
      strong_disj
        (scalar_mult (Q.( // ) 1 4) (var 1))
        (scalar_mult (Q.( // ) 1 2) (var 2)))
  in
  let t2 =
    Mu_term.(
      strong_disj
        (scalar_mult (Q.( // ) 1 4) (var 2))
        (scalar_mult (Q.( // ) 1 2) (var 3)))
  in
  let t3 =
    Mu_term.(
      strong_disj (const (Q.( // ) 1 4)) (scalar_mult (Q.( // ) 1 2) (var 4)))
  in
  let f = make_system 4 [| t1; t2; t3 |] in
  let point = Point.from_list [ Q.( // ) 1 2 ] in
  let result = System_solver.lfp_system 4 3 f point in
  (* Cle_vec.print result; *)
  let e1 = Cle_vec.expr result 1
  and e2 = Cle_vec.expr result 2
  and e3 = Cle_vec.expr result 3 in
  Q.equal (Lin_expr.eval e1 point) (Q.( // ) 1 4)
  && Q.equal (Lin_expr.eval e2 point) (Q.( // ) 1 8)
  && Q.equal (Lin_expr.eval e3 point) (Q.( // ) 1 2)
  (* && constraints_ok result point *)

(* ---- codim=4, chain ----
   Expected at x₁=1/2: x₂ = 1/4, x₃ = 1/8, x₄ = 1/16, x₅ = 1/2 *)

let%test "lfp_system codim=4 chain" =
  let t1 =
    Mu_term.(
      strong_disj
        (scalar_mult (Q.( // ) 1 4) (var 1))
        (scalar_mult (Q.( // ) 1 2) (var 2)))
  in
  let t2 =
    Mu_term.(
      strong_disj
        (scalar_mult (Q.( // ) 1 4) (var 2))
        (scalar_mult (Q.( // ) 1 2) (var 3)))
  in
  let t3 =
    Mu_term.(
      strong_disj
        (scalar_mult (Q.( // ) 1 4) (var 3))
        (scalar_mult (Q.( // ) 1 2) (var 4)))
  in
  let t4 =
    Mu_term.(
      strong_disj (const (Q.( // ) 1 4)) (scalar_mult (Q.( // ) 1 2) (var 5)))
  in
  let f = make_system 5 [| t1; t2; t3; t4 |] in
  let point = Point.from_list [ Q.( // ) 1 2 ] in
  let result = System_solver.lfp_system 5 4 f point in
  Cle_vec.print result;
  let e1 = Cle_vec.expr result 1
  and e2 = Cle_vec.expr result 2
  and e3 = Cle_vec.expr result 3
  and e4 = Cle_vec.expr result 4 in
  Q.equal (Lin_expr.eval e1 point) (Q.( // ) 1 4)
  && Q.equal (Lin_expr.eval e2 point) (Q.( // ) 1 8)
  && Q.equal (Lin_expr.eval e3 point) (Q.( // ) 1 16)
  && Q.equal (Lin_expr.eval e4 point) (Q.( // ) 1 2)
  (* && constraints_ok result point *)

(* ---- codim=2, m=2 ----
   Expected at (x₁,x₂)=(1/2,1/4): x₃ = 3/8, x₄ = 1/2 *)

let%test "lfp_system codim=2 m=2" =
  let t1 =
    Mu_term.(
      strong_disj
        (strong_disj
           (scalar_mult (Q.( // ) 1 4) (var 1))
           (scalar_mult (Q.( // ) 1 4) (var 2)))
        (scalar_mult (Q.( // ) 1 2) (var 3)))
  in
  let t2 =
    Mu_term.(
      strong_disj (const (Q.( // ) 1 4)) (scalar_mult (Q.( // ) 1 2) (var 4)))
  in
  let f = make_system 4 [| t1; t2 |] in
  let point = Point.from_list [ Q.( // ) 1 2; Q.( // ) 1 4 ] in
  let result = System_solver.lfp_system 4 2 f point in
  let e1 = Cle_vec.expr result 1 and e2 = Cle_vec.expr result 2 in
  Q.equal (Lin_expr.eval e1 point) (Q.( // ) 3 8)
  && Q.equal (Lin_expr.eval e2 point) (Q.( // ) 1 2)
  (* && constraints_ok result point *)
