open Linear

(** [component f k] extracts the [k]-th component of vector local algorithm [f]
    as a scalar [Local_alg.t] (1-indexed). *)
let component (f : Local_alg_vec.t) (k : int) : Local_alg.t =
 fun point ->
  let cle_vec = f point in
  Cle.construct (Cle_vec.constraints cle_vec) (Cle_vec.expr cle_vec k)

(** [substitute_component f_j g_i current_dim] returns a new [Local_alg.t] that
    expects [(current_dim - 1)]-dimensional points. It evaluates [g_i] to obtain
    an expression, extends the point to [current_dim] dimensions, calls [f_j],
    and substitutes [x_{current_dim}] with [g_i]'s expression in the result. *)
let substitute_component (f_j : Local_alg.t) (g_i : Local_alg.t)
    (current_dim : int) : Local_alg.t =
 fun point ->
  let g_cle = g_i point in
  let g_expr = Cle.expr g_cle in
  let g_constraints = Cle.constraints g_cle in
  let g_val = Lin_expr.eval g_expr point in
  let extended_point = Point.extend_dim point g_val in
  let f_cle = f_j extended_point in
  let f_expr = Cle.expr f_cle in
  let f_constraints = Cle.constraints f_cle in
  let new_expr = Lin_expr.substitute f_expr current_dim g_expr in
  let new_constraints =
    List.map
      (fun ineq -> Lin_ineq.substitute ineq current_dim g_expr)
      f_constraints
  in
  Cle.construct (g_constraints @ new_constraints) new_expr

let solver (dim : int) (codim : int) (f : Local_alg_vec.t) (point : Point.t) :
    Cle_vec.t =
  if dim <> Point.dim point + codim then
    invalid_arg
      "Simple_system_solver.solver: dim must equal Point.dim point + codim";

  let m = dim - codim in

  let components = Array.init codim (fun j -> component f (j + 1)) in

  let rec outer_loop i =
    if i < 1 then ()
    else
      let current_dim = m + i in
      let g_i = Algorithm.lfp current_dim components.(i - 1) in
      let rec inner_loop j =
        if j < 1 then ()
        else (
          if j = i then components.(j - 1) <- g_i
          else
            components.(j - 1) <-
              substitute_component components.(j - 1) g_i current_dim;
          inner_loop (j - 1))
      in
      inner_loop codim;
      outer_loop (i - 1)
  in
  outer_loop codim;

  let results = Array.map (fun comp -> comp point) components in
  let all_constraints =
    Array.fold_left (fun acc cle -> Cle.constraints cle @ acc) [] results
  in
  let exprs = Lin_expr_vec.from_array (Array.map Cle.expr results) in
  Cle_vec.construct all_constraints exprs
