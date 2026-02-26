open Core
open Linear
open Common

type t =
  | Var of int (* x_i *)
  | Zero
  | One
  | Scm of Q.t * t (* scalar multiplication *)
  | Lwd of t * t (* Lukasiewicz weak disjunction *)
  | Lwc of t * t (* Lukasiewicz weak conjunction *)
  | Lsd of t * t (* Lukasiewicz strong disjunction *)
  | Lsc of t * t (* Lukasiewicz strong conjunction *)
  | Mu of int * t (* lfp *)
  | Nu of int * t (* gfp *)

(* Constructors *)
let var i =
  if i < 1 then invalid_arg "Mu_term.var: index must be greater or equal than 1"
  else Var i

let const q =
  if Q.gt q Q.one || Q.lt q Q.zero then
    invalid_arg "Mu_term.const: constant must belong to the interval [0,1]"
  else if Q.equal q Q.zero then Zero
  else if Q.equal q Q.one then One
  else Scm (q, One)

let scalar_mult q term =
  if Q.gt q Q.one || Q.lt q Q.zero then
    invalid_arg
      "Mu_term.scalar_mult: constant must belong to the interval [0,1]"
  else if Q.equal q Q.zero then Zero
  else if Q.equal q Q.one then term
  else Scm (q, term)

let weak_disj term1 term2 =
  match (term1, term2) with
  | One, _ | _, One -> One
  | term1, term2 -> Lwd (term1, term2)

let weak_conj term1 term2 =
  match (term1, term2) with
  | Zero, _ | _, Zero -> Zero
  | term1, term2 -> Lwc (term1, term2)

let strong_disj term1 term2 =
  match (term1, term2) with
  | term1, Zero -> term1
  | Zero, term2 -> term2
  | _ -> Lsd (term1, term2)

let strong_conj term1 term2 =
  match (term1, term2) with
  | term1, One -> term1
  | One, term2 -> term2
  | _ -> Lsc (term1, term2)

let mu i term = Mu (i, term)
let nu i term = Nu (i, term)

let rec eval term point =
  let dim = Point.dim point in
  match term with
  | Var i ->
      let x = Lin_expr.x dim i in

      let ineqs =
        [
          Lin_ineq.construct (Lin_expr.zero dim) x Lin_ineq.LessEqual;
          Lin_ineq.construct x (Lin_expr.one dim) Lin_ineq.LessEqual;
        ]
      in
      Cond_lin_expr.construct ineqs x
  (* TODO: Lin_expr.zero and Lin_expr.one without dim *)
  | Zero -> Cond_lin_expr.construct [] (Lin_expr.zero dim)
  | One -> Cond_lin_expr.construct [] (Lin_expr.one dim)
  | Scm (q, term) ->
      let cle = eval term point in

      (* TODO: add Cond_lin_expr.mul_by *)
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

      let expr = Lin_expr.add expr1 expr2 and one = Lin_expr.one dim in
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
          (Lin_expr.mul_by Q.minus_one (Lin_expr.one dim))
      and zero = Lin_expr.zero dim in
      let ineq = Lin_ineq.construct expr zero Lin_ineq.GreaterEqual in
      if Lin_ineq.is_satisfied ineq point then
        Cond_lin_expr.construct ((ineq :: constraints1) @ constraints2) expr
      else
        Cond_lin_expr.construct
          ((Lin_ineq.reverse ineq :: constraints1) @ constraints2)
          zero
  | Mu (_, term) -> Algorithm.lfp (eval term) point
  | Nu (_, term) -> Algorithm.gfp (eval term) point

(* Print *)
let term_to_op = function
  | Var _ | Zero | One | Scm (_, _) -> ""
  | Lwd (_, _) -> " ⊔ "
  | Lwc (_, _) -> " ⊓ "
  | Lsd (_, _) -> " ⊕ "
  | Lsc (_, _) -> " ⊙ "
  | Mu (i, _) -> "μx" ^ Pprint.int_to_subscript i ^ "."
  | Nu (i, _) -> "vx" ^ Pprint.int_to_subscript i ^ "."

let rec to_string term =
  let bracket b = function
    | Var _ | Zero | One | Scm (_, Zero) | Scm (_, One) | Scm (_, Var _) -> ""
    | _ -> b
  in
  let left_bracket = bracket "(" in
  let right_bracket = bracket ")" in
  let op = term_to_op term in
  match term with
  | Var i -> "x" ^ Pprint.int_to_subscript i
  | Zero | Scm (_, Zero) -> "0"
  | One -> "1"
  | Scm (q, One) -> Q.to_string q
  | Scm (q, term) ->
      let s = Q.to_string q in
      s ^ " " ^ left_bracket term ^ to_string term ^ right_bracket term
  | Lwd (term1, term2)
  | Lwc (term1, term2)
  | Lsd (term1, term2)
  | Lsc (term1, term2) ->
      left_bracket term1 ^ to_string term1 ^ right_bracket term1 ^ op
      ^ left_bracket term2 ^ to_string term2 ^ right_bracket term2
  | Mu (_, term) | Nu (_, term) -> op ^ to_string term

let print term = print_string (to_string term)
