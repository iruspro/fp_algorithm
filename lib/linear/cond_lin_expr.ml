type t = { constraints : Lin_ineq.t list; expr : Lin_expr.t }

let construct constraints expr = { constraints; expr }
let constraints cle = cle.constraints
let expr cle = cle.expr

let dual dim cle =
  let dual_ineq ineq =
    let lhs = Lin_ineq.lhs ineq
    and rhs = Lin_ineq.rhs ineq
    and rel = Lin_ineq.rel ineq in

    (* 1 - x_n *)
    let expr =
      Lin_expr.add (Lin_expr.one dim)
        (Lin_expr.mul_by Q.minus_one (Lin_expr.x dim dim))
    in

    let lhs = Lin_expr.sub_last lhs expr and rhs = Lin_expr.sub_last rhs expr in

    Lin_ineq.construct dim lhs rhs rel
  in

  let dual_expr expr =
    let q_n =
      match expr with [] -> assert false | [ _ ] -> Q.zero | q_n :: _ -> q_n
    in
    let rec aux acc = function
      | [] -> List.rev acc
      | [ q_0 ] -> aux (Q.sub (Q.sub Q.one q_0) q_n :: acc) []
      | q_n :: qs when List.length qs = dim -> aux (q_n :: acc) qs
      | q :: qs -> aux (Q.neg q :: acc) qs
    in
    aux [] expr
  in

  let constraints = List.map dual_ineq (constraints cle)
  and expr = Lin_expr.from_list (dual_expr (Lin_expr.as_list (expr cle))) in
  construct constraints expr
