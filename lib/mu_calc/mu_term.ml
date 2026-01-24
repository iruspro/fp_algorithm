open Core
open Linear

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

let rec eval n_vars term point =
  match term with
  | Var i ->
      let x = Lin_expr.x n_vars i in

      let ineqs =
        [
          Lin_ineq.construct n_vars (Lin_expr.zero n_vars) x Lin_ineq.LessEqual;
          Lin_ineq.construct n_vars x (Lin_expr.one n_vars) Lin_ineq.LessEqual;
        ]
      in
      Cond_lin_expr.construct ineqs x
  | Zero -> Cond_lin_expr.construct [] (Lin_expr.zero n_vars)
  | One -> Cond_lin_expr.construct [] (Lin_expr.one n_vars)
  | Scm (q, term) ->
      let cle = eval n_vars term point in

      let constraints = Cond_lin_expr.constraints cle
      and expr = Cond_lin_expr.expr cle in
      Cond_lin_expr.construct constraints (Lin_expr.mul_by q expr)
  | Lwd (term1, term2) ->
      let cle1 = eval n_vars term1 point and cle2 = eval n_vars term2 point in

      let constraints1 = Cond_lin_expr.constraints cle1
      and expr1 = Cond_lin_expr.expr cle1
      and constraints2 = Cond_lin_expr.constraints cle2
      and expr2 = Cond_lin_expr.expr cle2 in

      let ineq = Lin_ineq.construct n_vars expr1 expr2 Lin_ineq.LessEqual in
      if Lin_ineq.is_satisfied ineq point then
        Cond_lin_expr.construct ((ineq :: constraints1) @ constraints2) expr2
      else
        Cond_lin_expr.construct
          ((Lin_ineq.reverse ineq :: constraints1) @ constraints2)
          expr1
  | Lwc (term1, term2) ->
      let cle1 = eval n_vars term1 point and cle2 = eval n_vars term2 point in

      let constraints1 = Cond_lin_expr.constraints cle1
      and expr1 = Cond_lin_expr.expr cle1
      and constraints2 = Cond_lin_expr.constraints cle2
      and expr2 = Cond_lin_expr.expr cle2 in

      let ineq = Lin_ineq.construct n_vars expr1 expr2 Lin_ineq.LessEqual in
      if Lin_ineq.is_satisfied ineq point then
        Cond_lin_expr.construct (ineq :: (constraints1 @ constraints2)) expr1
      else
        Cond_lin_expr.construct
          (Lin_ineq.reverse ineq :: (constraints1 @ constraints2))
          expr2
  | Lsd (term1, term2) ->
      let cle1 = eval n_vars term1 point and cle2 = eval n_vars term2 point in

      let constraints1 = Cond_lin_expr.constraints cle1
      and expr1 = Cond_lin_expr.expr cle1
      and constraints2 = Cond_lin_expr.constraints cle2
      and expr2 = Cond_lin_expr.expr cle2 in

      let expr = Lin_expr.add expr1 expr2 and one = Lin_expr.one n_vars in
      let ineq = Lin_ineq.construct n_vars expr one Lin_ineq.LessEqual in
      if Lin_ineq.is_satisfied ineq point then
        Cond_lin_expr.construct ((ineq :: constraints1) @ constraints2) expr
      else
        Cond_lin_expr.construct
          ((Lin_ineq.reverse ineq :: constraints1) @ constraints2)
          one
  | Lsc (term1, term2) ->
      let cle1 = eval n_vars term1 point and cle2 = eval n_vars term2 point in

      let constraints1 = Cond_lin_expr.constraints cle1
      and expr1 = Cond_lin_expr.expr cle1
      and constraints2 = Cond_lin_expr.constraints cle2
      and expr2 = Cond_lin_expr.expr cle2 in

      let expr =
        Lin_expr.add (Lin_expr.add expr1 expr2)
          (Lin_expr.mul_by Q.minus_one (Lin_expr.one n_vars))
      and zero = Lin_expr.zero n_vars in
      let ineq = Lin_ineq.construct n_vars expr zero Lin_ineq.GreaterEqual in
      if Lin_ineq.is_satisfied ineq point then
        Cond_lin_expr.construct ((ineq :: constraints1) @ constraints2) expr
      else
        Cond_lin_expr.construct
          ((Lin_ineq.reverse ineq :: constraints1) @ constraints2)
          zero
  | Mu term -> Algorithm.lfp n_vars (eval n_vars term) point
  | Nu term -> Algorithm.gfp n_vars (eval n_vars term) point
