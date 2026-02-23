open Core
open Linear
open Utils

type t =
  | Var of int (* x_i *)
  | Zero
  | One
  | Scm of Q.t * t (* scalar multiplication *)
  | Lwd of t * t (* Lukasiewicz weak disjunction *)
  | Lwc of t * t (* Lukasiewicz weak conjunction *)
  | Lsd of t * t (* Lukasiewicz strong disjunction *)
  | Lsc of t * t (* Lukasiewicz strong conjunction *)
  | Mu of t (* lfp *)
  | Nu of t (* gfp *)

let n_free_vars term =
  let rec aux_binded b_acc = function
    | Var _ | Zero | One -> b_acc
    | Scm (_, term) -> aux_binded b_acc term
    | Mu term | Nu term -> aux_binded (succ b_acc) term
    | Lwd (term1, term2)
    | Lwc (term1, term2)
    | Lsd (term1, term2)
    | Lsc (term1, term2) ->
        aux_binded b_acc term1 + aux_binded 0 term2
  in
  let rec aux_vars v_acc = function
    | Var _ -> v_acc + 1
    | Zero | One -> v_acc
    | Scm (_, term) | Mu term | Nu term -> aux_vars v_acc term
    | Lwd (term1, term2)
    | Lwc (term1, term2)
    | Lsd (term1, term2)
    | Lsc (term1, term2) ->
        aux_vars v_acc term1 + aux_vars 0 term2
  in
  aux_vars 0 term - aux_binded 0 term

let rec eval term point =
  let n_vars = Point.dim point in
  match term with
  | Var i ->
      let x = Lin_expr.x n_vars i in

      let ineqs =
        [
          Lin_ineq.construct (Lin_expr.zero n_vars) x Lin_ineq.LessEqual;
          Lin_ineq.construct x (Lin_expr.one n_vars) Lin_ineq.LessEqual;
        ]
      in
      Cond_lin_expr.construct ineqs x
  | Zero -> Cond_lin_expr.construct [] (Lin_expr.zero n_vars)
  | One -> Cond_lin_expr.construct [] (Lin_expr.one n_vars)
  | Scm (q, term) ->
      let cle = eval term point in

      let constraints = Cond_lin_expr.constraints cle
      and expr = Cond_lin_expr.expr cle in
      Cond_lin_expr.construct constraints (Lin_expr.mul_by q expr)
  | Lwd (term1, term2) ->
      let cle1 = eval term1 point and cle2 = eval term2 point in

      let constraints1 = Cond_lin_expr.constraints cle1
      and expr1 = Cond_lin_expr.expr cle1
      and constraints2 = Cond_lin_expr.constraints cle2
      and expr2 = Cond_lin_expr.expr cle2 in

      let ineq = Lin_ineq.construct expr1 expr2 Lin_ineq.LessEqual in
      if Lin_ineq.is_satisfied ineq point then
        Cond_lin_expr.construct ((ineq :: constraints1) @ constraints2) expr2
      else
        Cond_lin_expr.construct
          ((Lin_ineq.reverse ineq :: constraints1) @ constraints2)
          expr1
  | Lwc (term1, term2) ->
      let cle1 = eval term1 point and cle2 = eval term2 point in

      let constraints1 = Cond_lin_expr.constraints cle1
      and expr1 = Cond_lin_expr.expr cle1
      and constraints2 = Cond_lin_expr.constraints cle2
      and expr2 = Cond_lin_expr.expr cle2 in

      let ineq = Lin_ineq.construct expr1 expr2 Lin_ineq.LessEqual in
      if Lin_ineq.is_satisfied ineq point then
        Cond_lin_expr.construct (ineq :: (constraints1 @ constraints2)) expr1
      else
        Cond_lin_expr.construct
          (Lin_ineq.reverse ineq :: (constraints1 @ constraints2))
          expr2
  | Lsd (term1, term2) ->
      let cle1 = eval term1 point and cle2 = eval term2 point in

      let constraints1 = Cond_lin_expr.constraints cle1
      and expr1 = Cond_lin_expr.expr cle1
      and constraints2 = Cond_lin_expr.constraints cle2
      and expr2 = Cond_lin_expr.expr cle2 in

      let expr = Lin_expr.add expr1 expr2 and one = Lin_expr.one n_vars in
      let ineq = Lin_ineq.construct expr one Lin_ineq.LessEqual in
      if Lin_ineq.is_satisfied ineq point then
        Cond_lin_expr.construct ((ineq :: constraints1) @ constraints2) expr
      else
        Cond_lin_expr.construct
          ((Lin_ineq.reverse ineq :: constraints1) @ constraints2)
          one
  | Lsc (term1, term2) ->
      let cle1 = eval term1 point and cle2 = eval term2 point in

      let constraints1 = Cond_lin_expr.constraints cle1
      and expr1 = Cond_lin_expr.expr cle1
      and constraints2 = Cond_lin_expr.constraints cle2
      and expr2 = Cond_lin_expr.expr cle2 in

      let expr =
        Lin_expr.add (Lin_expr.add expr1 expr2)
          (Lin_expr.mul_by Q.minus_one (Lin_expr.one n_vars))
      and zero = Lin_expr.zero n_vars in
      let ineq = Lin_ineq.construct expr zero Lin_ineq.GreaterEqual in
      if Lin_ineq.is_satisfied ineq point then
        Cond_lin_expr.construct ((ineq :: constraints1) @ constraints2) expr
      else
        Cond_lin_expr.construct
          ((Lin_ineq.reverse ineq :: constraints1) @ constraints2)
          zero
  | Mu term -> Algorithm.lfp (eval term) point
  | Nu term -> Algorithm.gfp (eval term) point

let rec to_string term =
  match term with
  | Var i -> "x" ^ Pprint.int_to_subscript i
  | Zero -> "0"
  | One -> "1"
  | Scm (q, term) ->
      let s = Q.to_string q in
      s ^ "(" ^ to_string term ^ ")"
  | Lwd (term1, term2) ->
      "(" ^ to_string term1 ^ ") ⊔ (" ^ to_string term2 ^ ")"
  | Lwc (term1, term2) ->
      "(" ^ to_string term1 ^ ") ⊓ (" ^ to_string term2 ^ ")"
  | Lsd (term1, term2) ->
      "(" ^ to_string term1 ^ ") ⊕ (" ^ to_string term2 ^ ")"
  | Lsc (term1, term2) ->
      "(" ^ to_string term1 ^ ") ⊙ (" ^ to_string term2 ^ ")"
  | Mu term -> "μy.(" ^ to_string term ^ ")"
  | Nu term -> "vy.(" ^ to_string term ^ ")"

let print term = print_string (to_string term)
