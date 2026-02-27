open Linear

let bounds rel dim constraints =
  List.map Lin_ineq.rhs
    (List.filter
       (fun ineq -> Lin_ineq.dim ineq = dim && Lin_ineq.rel ineq = rel)
       constraints)

let strict_lowers = bounds Lin_ineq.Gt
let non_strict_lowers = bounds Lin_ineq.Ge
let strict_uppers = bounds Lin_ineq.Lt
let non_strict_uppers = bounds Lin_ineq.Le

let make_constraints lhs all_rhs rel =
  let rec aux acc = function
    | [] -> acc
    | rhs :: all_rhs -> aux (Lin_ineq.construct lhs rhs rel :: acc) all_rhs
  in
  aux [] all_rhs

let rec find_unsatisfied ineqs point =
  match ineqs with
  | [] -> None
  | ineq :: ineqs ->
      if not (Lin_ineq.is_satisfied ineq point) then Some ineq
      else find_unsatisfied ineqs point

let find_supremum_term terms point =
  let rec aux c_term c_min = function
    | [] -> c_term
    | term :: terms ->
        let c_val = Lin_expr.eval term point in
        if Q.lt c_val c_min then aux (Some term) c_val terms
        else aux c_term c_min terms
  in
  aux None Q.inf terms

let lfp dim local_alg (* t *) point (* r *) =
  (* Sanity check *)
  assert (pred dim = Point.dim point);

  let new_dim = Point.dim point in
  let zero = Lin_expr.const Q.zero in

  let find_next_approximation c_approx constraints' n strict_lowers
      non_strict_lowers strict_uppers non_strict_uppers expr =
    let c_expr (* d *) = Cond_lin_expr.expr c_approx in

    (* Find supremum term *)
    let sup_term (* b_j *), from_strict =
      match find_supremum_term (strict_uppers @ non_strict_uppers) point with
      | Some term -> (term, List.mem term strict_uppers)
      | None -> assert false
    in

    (* Define new constraints *)
    let constraints =
      if from_strict then
        (* C' + {N} + {d < b_j} *)
        (Lin_ineq.construct c_expr sup_term Lin_ineq.Lt :: n :: constraints')
        (* {b_j <= b_i} *)
        @ make_constraints sup_term non_strict_uppers Lin_ineq.Le
        (* {d > a_i} *)
        @ make_constraints c_expr strict_lowers Lin_ineq.Gt
        (* {d >= a_i} *)
        @ make_constraints c_expr non_strict_lowers Lin_ineq.Ge
      else
        (* C' + {N} + {d <= d_j} *)
        (Lin_ineq.construct c_expr sup_term Lin_ineq.Le :: n :: constraints')
        (* {b_j < b_i} *)
        @ make_constraints sup_term strict_uppers Lin_ineq.Lt
        (* {b_j <= b_i} *)
        @ make_constraints sup_term non_strict_uppers Lin_ineq.Le
        (* {d > a_i} *)
        @ make_constraints c_expr strict_lowers Lin_ineq.Gt
        (* {d >= a_i} *)
        @ make_constraints c_expr non_strict_lowers Lin_ineq.Ge
    in

    (* Define a new linear expression *)
    let expr = Lin_expr.substitute expr dim sup_term in

    Cond_lin_expr.with_expr
      (Cond_lin_expr.add_constraints c_approx constraints)
      expr
  in

  (* loop *)
  let rec aux c_approx (* D ⊢ d *) =
    (* d *)
    let c_expr = Cond_lin_expr.expr c_approx in

    (* C ⊢ e = t(r, d(r))*)
    let cle = local_alg (Point.extend_dim point (Lin_expr.eval c_expr point)) in
    (* C *)
    let constraints = Cond_lin_expr.constraints cle
    (* e *)
    and expr = Cond_lin_expr.expr cle in

    (* Arrange inequalities in C *)
    (* C' *)
    let constraints' =
      List.filter (fun ineq -> Lin_ineq.dim ineq = new_dim) constraints
    in
    (* Get lower/upper bounds for x_{n + 1} *)
    (* {a_i | x_{n + 1} > a_i} *)
    let strict_lowers = strict_lowers dim constraints
    (* {a_i | x_{n + 1} >= a_i} *)
    and non_strict_lowers = non_strict_lowers dim constraints
    (* {b_i | x_{n + 1} < b_i} *)
    and strict_uppers = strict_uppers dim constraints
    (* {b_i | x_{n + 1} <= b_i} *)
    and non_strict_uppers = non_strict_uppers dim constraints in

    (* Check linear expression *)
    (* q_{n + 1} *)
    let q = Lin_expr.coeff expr dim in
    (* q_{n + 1} x_{n + 1} *)
    let qx = Lin_expr.mul_by q (Lin_expr.x dim) in
    let reduced = Lin_expr.sub expr qx in

    if not (Q.equal q Q.one) then
      (* Define a new linear expression *)
      (* 1 / (1 - q_{n + 1}) *)
      let c = Q.inv (Q.sub Q.one q) in
      let f = Lin_expr.mul_by c reduced in

      (* Check if C(r, f(r)) holds *)
      let fail =
        find_unsatisfied constraints
          (Point.extend_dim point (Lin_expr.eval f point))
      in
      match fail with
      | None (* C(r, f(r)) holds *) ->
          (* Define new constraints *)
          (* d <= f *)
          let inv_constraint = Lin_ineq.construct c_expr f Lin_ineq.Le
          and
              (* {d > a_i} *)
              strict_lowers =
            make_constraints c_expr strict_lowers Lin_ineq.Gt
          and
              (* {d >= a_i} *)
              non_strict_lowers =
            make_constraints c_expr non_strict_lowers Lin_ineq.Ge
          and
              (* {f < b_i} *)
              strict_uppers =
            make_constraints f strict_uppers Lin_ineq.Lt
          and
              (* {f <= b_i} *)
              non_strict_uppers =
            make_constraints f non_strict_uppers Lin_ineq.Le
          in

          let cle =
            Cond_lin_expr.add_constraints cle
              ((inv_constraint :: constraints')
              @ strict_lowers @ non_strict_lowers @ strict_uppers
              @ non_strict_uppers)
          in
          (* Exit the loop with E ⊢ f *)
          Cond_lin_expr.with_expr cle f
      | Some ineq (* C(r, f(r)) doesn't holds *) ->
          (* Define new constraint N(x_1, ..., x_n) *)
          let n = Lin_ineq.negate (Lin_ineq.substitute ineq dim f) in
          aux
            (find_next_approximation c_approx constraints' n strict_lowers
               non_strict_lowers strict_uppers non_strict_uppers expr)
    else
      (* q_{n + 1} = 1 *)
      let v = Lin_expr.eval reduced point in

      if Q.equal v Q.zero then
        (* q_n r_n + ... + q_1 r_1 + q_0 = 0 *)
        (* Define new constraints *)
        let constraints =
          List.map (fun ineq -> Lin_ineq.substitute ineq dim c_expr) constraints
        and eq_zero =
          [
            Lin_ineq.construct reduced zero Lin_ineq.Le;
            Lin_ineq.construct reduced zero Lin_ineq.Ge;
          ]
        in
        (* Exit the loop with E ⊢ d *)
        Cond_lin_expr.add_constraints c_approx (constraints @ eq_zero)
      else
        (* q_n r_n + ... + q_1 r_1 + q_0 <> 0 *)
        (* Define new constraint N(x_1, ..., x_n) *)
        let ineq = Lin_ineq.construct reduced zero in
        let n = if Q.lt v Q.zero then ineq Lin_ineq.Lt else ineq Lin_ineq.Gt in
        aux
          (find_next_approximation c_approx constraints' n strict_lowers
             non_strict_lowers strict_uppers non_strict_uppers expr)
  in
  aux (Cond_lin_expr.construct [] zero)

let gfp dim (local_alg : Local_alg.t) point =
  let cle = lfp dim (Utils.dual local_alg) point in
  let expr = Utils.complement (Cond_lin_expr.expr cle) in
  Cond_lin_expr.with_expr cle expr
