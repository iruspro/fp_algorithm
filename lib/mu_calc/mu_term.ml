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

let rec eval dim term point =
  (* Sanity check *)
  assert (dim = Point.dim point);

  match term with
  | Var i ->
      let x = Lin_expr.x i in

      let ineqs =
        [
          Lin_ineq.construct (Lin_expr.const Q.zero) x Lin_ineq.Le;
          Lin_ineq.construct x (Lin_expr.const Q.one) Lin_ineq.Le;
        ]
      in
      Cond_lin_expr.construct ineqs x
  (* TODO: Lin_expr.zero and Lin_expr.one without dim *)
  | Zero -> Cond_lin_expr.construct [] (Lin_expr.const Q.zero)
  | One -> Cond_lin_expr.construct [] (Lin_expr.const Q.one)
  | Scm (q, term) ->
      let cle = eval dim term point in

      (* TODO: add Cond_lin_expr.mul_by *)
      let constraints = Cond_lin_expr.constraints cle
      and expr = Cond_lin_expr.expr cle in
      Cond_lin_expr.construct constraints (Lin_expr.mul_by q expr)
  | Lwd (term1, term2) ->
      let cle1 = eval dim term1 point and cle2 = eval dim term2 point in

      let constraints1 = Cond_lin_expr.constraints cle1
      and expr1 = Cond_lin_expr.expr cle1
      and constraints2 = Cond_lin_expr.constraints cle2
      and expr2 = Cond_lin_expr.expr cle2 in

      let ineq = Lin_ineq.construct expr1 expr2 in
      if Lin_ineq.is_satisfied (ineq Lin_ineq.Le) point then
        Cond_lin_expr.construct
          ((ineq Lin_ineq.Le :: constraints1) @ constraints2)
          expr2
      else
        Cond_lin_expr.construct
          ((ineq Lin_ineq.Ge :: constraints1) @ constraints2)
          expr1
  | Lwc (term1, term2) ->
      let cle1 = eval dim term1 point and cle2 = eval dim term2 point in

      let constraints1 = Cond_lin_expr.constraints cle1
      and expr1 = Cond_lin_expr.expr cle1
      and constraints2 = Cond_lin_expr.constraints cle2
      and expr2 = Cond_lin_expr.expr cle2 in

      let ineq = Lin_ineq.construct expr1 expr2 in
      if Lin_ineq.is_satisfied (ineq Lin_ineq.Le) point then
        Cond_lin_expr.construct
          (ineq Lin_ineq.Le :: (constraints1 @ constraints2))
          expr1
      else
        Cond_lin_expr.construct
          (ineq Lin_ineq.Ge :: (constraints1 @ constraints2))
          expr2
  | Lsd (term1, term2) ->
      let cle1 = eval dim term1 point and cle2 = eval dim term2 point in

      let constraints1 = Cond_lin_expr.constraints cle1
      and expr1 = Cond_lin_expr.expr cle1
      and constraints2 = Cond_lin_expr.constraints cle2
      and expr2 = Cond_lin_expr.expr cle2 in

      let expr = Lin_expr.add expr1 expr2 and one = Lin_expr.const Q.one in
      let ineq = Lin_ineq.construct expr one in
      if Lin_ineq.is_satisfied (ineq Lin_ineq.Le) point then
        Cond_lin_expr.construct
          ((ineq Lin_ineq.Le :: constraints1) @ constraints2)
          expr
      else
        Cond_lin_expr.construct
          ((ineq Lin_ineq.Ge :: constraints1) @ constraints2)
          one
  | Lsc (term1, term2) ->
      let cle1 = eval dim term1 point and cle2 = eval dim term2 point in

      let constraints1 = Cond_lin_expr.constraints cle1
      and expr1 = Cond_lin_expr.expr cle1
      and constraints2 = Cond_lin_expr.constraints cle2
      and expr2 = Cond_lin_expr.expr cle2 in

      let expr =
        Lin_expr.add (Lin_expr.add expr1 expr2)
          (Lin_expr.mul_by Q.minus_one (Lin_expr.const Q.one))
      and zero = Lin_expr.const Q.zero in
      let ineq = Lin_ineq.construct expr zero in
      if Lin_ineq.is_satisfied (ineq Lin_ineq.Ge) point then
        Cond_lin_expr.construct
          ((ineq Lin_ineq.Ge :: constraints1) @ constraints2)
          expr
      else
        Cond_lin_expr.construct
          ((ineq Lin_ineq.Le :: constraints1) @ constraints2)
          zero
  | Mu (_, term) -> Algorithm.lfp (succ dim) (eval (succ dim) term) point
  | Nu (_, term) -> Algorithm.gfp (succ dim) (eval (succ dim) term) point

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

(* TESTS *)
let mu_term =
  (* μx₂.(5/8 ⊕ 3/8 x₁) ⊙ (1/2 ⊔ (3/8 ⊕ 1/2 x₂)) *)
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
  Q.equal fp (Q.( // ) 3 4) && true

(* PRINT *)
let%test "to_string" =
  to_string mu_term = "μx₂.(5/8 ⊕ 3/8 x₁) ⊙ (1/2 ⊔ (3/8 ⊕ 1/2 x₂))"
