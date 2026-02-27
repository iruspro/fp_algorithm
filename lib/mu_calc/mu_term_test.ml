open Linear
open Mu_term

let mu_term =
  mu 2
    (strong_conj
       (strong_disj (const (Q.( // ) 5 8)) (scalar_mult (Q.( // ) 3 8) (var 1)))
       (weak_disj
          (const (Q.( // ) 1 2))
          (strong_disj
             (const (Q.( // ) 3 8))
             (scalar_mult (Q.( // ) 1 2) (var 2)))))

(* FUNCTIONS *)
let%test "eval" =
  let point = Point.from_list [ Q.one ] in
  let cle = eval 1 mu_term point in
  let constraints = Cond_lin_expr.constraints cle
  and expr = Cond_lin_expr.expr cle in
  let fp = Lin_expr.eval expr point
  and _ =
    List.fold_left ( && ) true
      (List.map (fun ineq -> Lin_ineq.is_satisfied ineq point) constraints)
  in
  (* print_endline "\nTEST";
  Cond_lin_expr.print cle;
  print_endline ""; *)
  Q.equal fp (Q.( // ) 3 4) && true

(* PRINT *)
let%test "to_string" =
  to_string mu_term = "μx₂.(5/8 ⊕ 3/8 x₁) ⊙ (1/2 ⊔ (3/8 ⊕ 1/2 x₂))"
