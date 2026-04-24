open Linear

(** [init_cle dim codim] returns a [Cle_vec.t] with constraints [0 ≤ x_i ≤ 1]
    for [i = 1, ..., dim] and [codim] zero expressions. *)
let init_cle dim codim =
  let zero = Lin_expr.const Q.zero and one = Lin_expr.const Q.one in
  let constraints =
    List.init dim (fun i ->
        let x = Lin_expr.x (i + 1) in
        [
          Lin_ineq.construct zero x Lin_ineq.Le;
          Lin_ineq.construct x one Lin_ineq.Le;
        ])
    |> List.concat
  in
  Cle_vec.construct constraints (Lin_expr_vec.const (Array.make codim Q.zero))

(** [substitute_prev d offset k] substitutes [x_{offset+j} := d_j] for
    [j = k, ..., 1] in all components of [d]. This eliminates [x_{offset+k}]
    from components with index [> k] (whose natural invariant allows it) while
    being a no-op for components with index [≤ k]. *)
let substitute_prev d offset k =
  let rec aux d j =
    if j < 1 then d
    else
      let d_j = Lin_expr_vec.expr d j in
      aux (Lin_expr_vec.substitute_from d 1 (offset + j) d_j) (j - 1)
  in
  aux d k

(** [merge_point point d] returns the point
    [(r_1, ..., r_m, d_1(r), ..., d_n(r))]. *)
let merge_point point d =
  let d_r = Lin_expr_vec.eval d point in
  Point.from_list (Point.as_list point @ Array.to_list d_r)

(** [substitute_constraints constraints indices subs] substitutes in each
    constraint [x_{indices.(j)} := subs.(j)] for [j = 0, ..., len-1]. *)
let substitute_constraints constraints indices subs =
  let n = Array.length indices in
  List.map
    (fun ineq ->
      let rec aux ineq j =
        if j >= n then ineq
        else aux (Lin_ineq.substitute ineq indices.(j) subs.(j)) (j + 1)
      in
      aux ineq 0)
    constraints

(** [bound_indices m k codim] returns [[| m+k; ...; m+codim |]]. *)
let bound_indices m k codim =
  let len = codim - k + 1 in
  Array.init len (fun j -> m + k + j)

(** [bound_subs exprs k codim] returns [[| d_k; ...; d_codim |]]. *)
let bound_subs exprs k codim =
  let len = codim - k + 1 in
  Array.init len (fun j -> Lin_expr_vec.expr exprs (k + j))

(** [bounds rel d constraints] returns the RHS expressions of constraints
    whose relation is [rel] and whose dimension is [d]. *)
let bounds rel d constraints =
  List.map Lin_ineq.rhs
    (List.filter
       (fun ineq -> Lin_ineq.dim ineq = d && Lin_ineq.rel ineq = rel)
       constraints)

let strict_lowers = bounds Lin_ineq.Gt
let non_strict_lowers = bounds Lin_ineq.Ge
let strict_uppers = bounds Lin_ineq.Lt
let non_strict_uppers = bounds Lin_ineq.Le

(** [make_constraints lhs all_rhs rel] constructs [lhs rel rhs] for every
    [rhs] in [all_rhs]. *)
let make_constraints lhs all_rhs rel =
  List.map (fun rhs -> Lin_ineq.construct lhs rhs rel) all_rhs

(** [find_unsatisfied ineqs point] returns the first inequality in [ineqs]
    that fails at [point], or [None] if all are satisfied. *)
let rec find_unsatisfied ineqs point =
  match ineqs with
  | [] -> None
  | ineq :: ineqs ->
      if not (Lin_ineq.is_satisfied ineq point) then Some ineq
      else find_unsatisfied ineqs point

(** [find_supremum_term terms point] returns the term in [terms] whose value
    at [point] is smallest. Ties resolve to the first occurrence. *)
let find_supremum_term terms point =
  let rec aux c_term c_min = function
    | [] -> c_term
    | term :: terms ->
        let c_val = Lin_expr.eval term point in
        if Q.lt c_val c_min then aux (Some term) c_val terms
        else aux c_term c_min terms
  in
  aux None Q.inf terms

(** [prefix_point point n] returns the point formed by the first [n]
    coordinates of [point]. *)
let prefix_point point n =
  Point.from_list (List.filteri (fun i _ -> i < n) (Point.as_list point))

let lfp_system dim codim local_alg point =
  if dim (* m + n *) <> Point.dim point + codim (* n *) then
    invalid_arg
      "System_solver.lfp_system: dim must equal Point.dim point + codim";

  let m = dim - codim in

  (* loop *)
  let rec aux k c_approx (* D ⊢ (d_1, ..., d_n) *) =
    match k with
    | 0 -> c_approx
    | k when k > 0 ->
        let c_exprs = Cle_vec.exprs c_approx in

        let d (* d(z) *) = substitute_prev c_exprs m k in
        let c_point (* (r, d(r)) *) = merge_point point d in

        let cle (* E ⊢ (f_1, ..., f_n) *) = local_alg c_point in
        let constraints (* E *) = Cle_vec.constraints cle
        and expr (* e *) = Cle_vec.expr cle k in

        if
          Q.equal
            (Lin_expr.eval expr c_point)
            (Lin_expr.eval (Lin_expr_vec.expr d k) point)
        then
          (* We are at a fixed point *)
          (* Update expressions: d_{k+1..n} := d_{k+1..n}[x_{m+k} := d_k]. After
             this, d_{k+1..n} no longer contain x_{m+k}, so subs below are all
             in z, x_{m+1..m+k-1} and the order of substitution no longer
             matters. *)
          let d_k = Lin_expr_vec.expr c_exprs k in
          let c_approx = Cle_vec.substitute_from c_approx (k + 1) (m + k) d_k in
          let c_exprs = Cle_vec.exprs c_approx in

          (* Update constraints *)
          let indices = bound_indices m k codim in
          let subs = bound_subs c_exprs k codim in
          let constraints' = substitute_constraints constraints indices subs in

          let expr' = Lin_expr.substitute_many expr indices subs in
          let inv_constraint = Lin_ineq.construct expr' d_k Lin_ineq.Le in

          let c_approx =
            Cle_vec.add_constraints c_approx (inv_constraint :: constraints')
          in

          (* Return to loop: D := D[x_{k-1} := d_{k-1}] (skipped when k = 1) *)
          let c_approx =
            if k > 1 then
              Cle_vec.substitute_constraints c_approx (m + k - 1)
                (Lin_expr_vec.expr c_exprs (k - 1))
            else c_approx
          in
          aux (k - 1) c_approx
        else
          (* Not at fixed point *)
          let indices_after = bound_indices m (k + 1) codim in
          let subs_after = bound_subs c_exprs (k + 1) codim in
          let tilde_e (* ẽ *) =
            Lin_expr.substitute_many expr indices_after subs_after
          and tilde_E (* Ẽ *) =
            substitute_constraints constraints indices_after subs_after
          in

          (* Arrange Ẽ as E' ∪ {a_i ◁ x_{m+k}} ∪ {x_{m+k} ◁ b_j} *)
          let constraints_E' =
            List.filter (fun ineq -> Lin_ineq.dim ineq < m + k) tilde_E
          in
          let sl = strict_lowers (m + k) tilde_E
          and nl = non_strict_lowers (m + k) tilde_E
          and su = strict_uppers (m + k) tilde_E
          and nu = non_strict_uppers (m + k) tilde_E in

          let d_k (* OLD d_k *) = Lin_expr_vec.expr c_exprs k in
          let q_k = Lin_expr.coeff tilde_e (m + k) in
          let qx = Lin_expr.mul_by q_k (Lin_expr.x (m + k)) in
          let reduced = Lin_expr.sub tilde_e qx in

          (* d_{k+1..n} := d_{k+1..n}[x_{m+k} := d_k] (OLD d_k) *)
          let c_approx_sub =
            Cle_vec.substitute_from c_approx (k + 1) (m + k) d_k
          in

          (* Reintroduction: {d_i ≤ x_{m+i} ≤ 1 | k ≤ i < codim} *)
          let reintro =
            let one = Lin_expr.const Q.one in
            let exprs_sub = Cle_vec.exprs c_approx_sub in
            let rec build acc i =
              if i >= codim then acc
              else
                let d_i = Lin_expr_vec.expr exprs_sub i in
                let x_i = Lin_expr.x (m + i) in
                build
                  (Lin_ineq.construct d_i x_i Lin_ineq.Le
                  :: Lin_ineq.construct x_i one Lin_ineq.Le
                  :: acc)
                  (i + 1)
            in
            build [] k
          in

          let prefix = prefix_point c_point (m + k - 1) in

          let next_approx_with n_constraint =
            let b_s, from_strict =
              match find_supremum_term (su @ nu) prefix with
              | Some term -> (term, List.exists (Lin_expr.equal term) su)
              | None -> assert false
            in
            let d_new = Lin_expr.substitute tilde_e (m + k) b_s in
            let bs_constraints =
              if from_strict then make_constraints b_s (su @ nu) Lin_ineq.Le
              else
                make_constraints b_s nu Lin_ineq.Le
                @ make_constraints b_s su Lin_ineq.Lt
            in
            let dk_bs_rel = if from_strict then Lin_ineq.Lt else Lin_ineq.Le in
            let dk_bs = Lin_ineq.construct d_k b_s dk_bs_rel in
            let lowers_d_k =
              make_constraints d_k sl Lin_ineq.Gt
              @ make_constraints d_k nl Lin_ineq.Ge
            in
            let new_constraints =
              (n_constraint :: dk_bs :: constraints_E')
              @ bs_constraints @ lowers_d_k @ reintro
            in
            let c_approx = Cle_vec.add_constraints c_approx_sub new_constraints in
            let c_approx = Cle_vec.with_expr c_approx d_new k in
            aux codim c_approx
          in

          if not (Q.equal q_k Q.one) then begin
            let g = Lin_expr.mul_by (Q.inv (Q.sub Q.one q_k)) reduced in
            let eval_point =
              Point.extend_dim prefix (Lin_expr.eval g c_point)
            in
            match find_unsatisfied tilde_E eval_point with
            | None ->
                (* Ẽ holds at (r, d̃_{1..k-1}(r), g(r, d̃_{1..k-1}(r))) *)
                let uppers_g =
                  make_constraints g su Lin_ineq.Lt
                  @ make_constraints g nu Lin_ineq.Le
                in
                let lowers_d_k =
                  make_constraints d_k sl Lin_ineq.Gt
                  @ make_constraints d_k nl Lin_ineq.Ge
                in
                let new_constraints =
                  constraints_E' @ lowers_d_k @ uppers_g @ reintro
                in
                let c_approx =
                  Cle_vec.add_constraints c_approx_sub new_constraints
                in
                let c_approx = Cle_vec.with_expr c_approx g k in
                aux codim c_approx
            | Some ineq ->
                let n_constraint =
                  Lin_ineq.negate (Lin_ineq.substitute ineq (m + k) g)
                in
                next_approx_with n_constraint
          end
          else
            let v = Lin_expr.eval reduced c_point in
            if Q.equal v Q.zero then assert false
            else
              let side = if Q.lt v Q.zero then Lin_ineq.Lt else Lin_ineq.Gt in
              let n_constraint =
                Lin_ineq.construct reduced (Lin_expr.const Q.zero) side
              in
              next_approx_with n_constraint
    | _ -> assert false
  in
  aux codim (init_cle dim codim)
