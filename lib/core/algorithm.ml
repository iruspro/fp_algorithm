open Linear

let lfp (local_alg : Local_alg.t) point (* r *) =
  let new_dim = Point.dim point in
  let zero = Lin_expr.zero new_dim
  and ineq_constructor = Lin_ineq.construct
  and constraints_maker = Lin_ineq.make_constraints in

  let sub_and_reduce expr1 expr2 =
    Lin_expr.reduce_dim (Lin_expr.sub_last expr1 expr2)
  in

  let find_next_approximation c_constraints constraints' n c_approx expr
      strict_lowers non_strict_lowers strict_uppers non_strict_uppers =
    let term, strict =
      match
        Lin_expr.find_supremum_term (strict_uppers @ non_strict_uppers) point
      with
      | Some term -> (term, List.mem term strict_uppers)
      | None -> failwith "Undefined error"
    in
    let c_constraints =
      if strict then
        c_constraints @ constraints' @ [ n ]
        @ constraints_maker c_approx [ term ] Lin_ineq.LessThan
        @ constraints_maker term non_strict_uppers Lin_ineq.LessEqual
        @ constraints_maker c_approx strict_lowers Lin_ineq.GreaterThan
        @ constraints_maker c_approx non_strict_lowers Lin_ineq.GreaterEqual
      else
        c_constraints @ constraints' @ [ n ]
        @ constraints_maker c_approx [ term ] Lin_ineq.LessEqual
        @ constraints_maker term strict_uppers Lin_ineq.LessThan
        @ constraints_maker term non_strict_uppers Lin_ineq.LessEqual
        @ constraints_maker c_approx strict_lowers Lin_ineq.GreaterThan
        @ constraints_maker c_approx non_strict_lowers Lin_ineq.GreaterEqual
    in
    (c_constraints, sub_and_reduce expr (Lin_expr.extend_dim term Q.zero))
  in

  let get_new_constraints constraints =
    List.map
      (fun ineq ->
        ineq_constructor
          (Lin_expr.reduce_dim (Lin_ineq.lhs ineq))
          (Lin_expr.reduce_dim (Lin_ineq.rhs ineq))
          (Lin_ineq.rel ineq))
      (List.filter
         (fun ineq -> Lin_ineq.n_type ineq = Lin_ineq.WithoutLast)
         constraints)
  in

  let get_terms constraints ineq_type =
    List.map Lin_expr.reduce_dim
      (Lin_ineq.extract_rh_sides
         (List.filter
            (fun ineq -> Lin_ineq.n_type ineq = ineq_type)
            constraints))
  in

  let reduce_constraints constraints c_approx =
    List.map
      (fun ineq ->
        ineq_constructor
          (sub_and_reduce (Lin_ineq.lhs ineq)
             (Lin_expr.extend_dim c_approx Q.zero))
          (sub_and_reduce (Lin_ineq.rhs ineq)
             (Lin_expr.extend_dim c_approx Q.zero))
          (Lin_ineq.rel ineq))
      constraints
  in

  (* loop *)
  let rec aux c_constraints c_approx =
    let cle =
      local_alg (Point.extend_dim point (Lin_expr.eval c_approx point))
    in
    let constraints (* C *) = Cond_lin_expr.constraints cle
    and expr (* e *) = Cond_lin_expr.expr cle in

    (* Arrange inequalities in C *)
    let constraints' (* C' *) = get_new_constraints constraints
    and strict_lowers (* {a_i | x_n > a_i} *) =
      get_terms constraints Lin_ineq.LastGreaterThan
    and non_strict_lowers (* {a_i | x_n >= a_i} *) =
      get_terms constraints Lin_ineq.LastGreaterEqual
    and strict_uppers (* {b_i | x_n < b_i} *) =
      get_terms constraints Lin_ineq.LastLessThan
    and non_strict_uppers (* {b_i | x_n <= b_i} *) =
      get_terms constraints Lin_ineq.LastLessEqual
    in

    (* Check linear expression *)
    match Lin_expr.as_list expr with
    | q_n :: qs when Q.( <> ) q_n Q.one (* q_n != 1 case *) -> (
        (* Define a new linear expression *)
        let f =
          Lin_expr.mul_by
            (Q.div Q.one (Q.sub Q.one q_n))
            (Lin_expr.from_list qs)
        in

        (* Check constraints at (r, f(r)) *)
        match
          Lin_ineq.find_unsatisfied constraints
            (Point.extend_dim point (Lin_expr.eval f point))
        with
        | None (* C(r, f(r)) holds case *) ->
            (* Update current constraints *)
            let c_constraints (* E *) =
              c_constraints (* D *) @ constraints' (* C' *)
              @ constraints_maker c_approx [ f ]
                  Lin_ineq.LessEqual (* {d <= f} *)
              @ constraints_maker c_approx strict_lowers
                  Lin_ineq.GreaterThan (* {d > a_i} *)
              @ constraints_maker c_approx non_strict_lowers
                  Lin_ineq.GreaterEqual (* {d >= a_i} *)
              @ constraints_maker f strict_uppers
                  Lin_ineq.LessThan (* {f < b_i} *)
              @ constraints_maker f non_strict_uppers
                  Lin_ineq.LessEqual (* {f <= b_i} *)
            in
            (* Exit the loop *)
            Cond_lin_expr.construct c_constraints f
        | Some ineq (* C(r, f(r)) doesn't hold case *) ->
            (* Define a new constraint and go to find next optimization *)
            let lhs =
              sub_and_reduce (Lin_ineq.lhs ineq) (Lin_expr.extend_dim f Q.zero)
            and rhs =
              sub_and_reduce (Lin_ineq.rhs ineq) (Lin_expr.extend_dim f Q.zero)
            in
            let n =
              Lin_ineq.negate (ineq_constructor lhs rhs (Lin_ineq.rel ineq))
            in
            let c_constraints, c_approx =
              find_next_approximation c_constraints constraints' n c_approx expr
                strict_lowers non_strict_lowers strict_uppers non_strict_uppers
            in
            aux c_constraints c_approx)
    | _ :: qs
      when Q.equal
             (Lin_expr.eval (Lin_expr.from_list qs) point)
             Q.zero (* q_n = 1 and q_n r_n + ... + r_1 q_1 + q_0 = 0 case *) ->
        (* Update current constraints *)
        let c_constraints =
          c_constraints (* D *)
          @ reduce_constraints constraints c_approx (* C(x, d(x)) *)
          @ [
              (* q_0 + q_1 x_1 + ... + q_{n-1} x_{n-1} = 0 *)
              ineq_constructor (Lin_expr.from_list qs) zero Lin_ineq.LessEqual;
              ineq_constructor (Lin_expr.from_list qs) zero
                Lin_ineq.GreaterEqual;
            ]
        in
        (* Exit the loop *)
        Cond_lin_expr.construct c_constraints c_approx
    | _ :: qs (* q_n = 1 and q_n r_n + ... + r_1 q_1 + q_0 != 0 case *) ->
        (* Define a new constraint and go to find next optimization *)
        let ineq1 =
          ineq_constructor (Lin_expr.from_list qs) zero Lin_ineq.LessThan
        and ineq2 =
          ineq_constructor (Lin_expr.from_list qs) zero Lin_ineq.GreaterThan
        in
        let n = if Lin_ineq.is_satisfied ineq1 point then ineq1 else ineq2 in
        let c_constraints, c_approx =
          find_next_approximation c_constraints constraints' n c_approx expr
            strict_lowers non_strict_lowers strict_uppers non_strict_uppers
        in
        aux c_constraints c_approx
    | [] -> failwith "A linear expression must have at least one coefficient"
  in
  aux [] zero

let gfp (local_alg : Local_alg.t) point =
  let cle = lfp (Local_alg.dual local_alg) point in
  let expr = Lin_expr.complement (Cond_lin_expr.expr cle) in
  Cond_lin_expr.with_expr cle expr
