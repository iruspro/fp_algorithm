type t = { constraints : Lin_ineq.t list; exprs : Lin_expr_vec.t }

(* CONSTRUCTORS *)
let construct constraints exprs = { constraints; exprs }

(* GETTERS *)
let constraints cle = cle.constraints
let exprs cle = cle.exprs

let expr cle k =
  try Lin_expr_vec.expr cle.exprs k
  with Invalid_argument _ -> invalid_arg "Cle_vec.expr: index out of bounds"

let codim cle = Lin_expr_vec.codim cle.exprs

(* FUNCTIONS *)
let add_constraint cle ineq = construct (ineq :: constraints cle) (exprs cle)
let add_constraints cle ineqs = construct (constraints cle @ ineqs) (exprs cle)

let with_expr cle e k =
  try construct (constraints cle) (Lin_expr_vec.with_expr (exprs cle) k e)
  with Invalid_argument _ ->
    invalid_arg "Cle_vec.with_expr: index out of bounds"

let with_exprs cle es = construct (constraints cle) es

let substitute_constraints cle i sub =
  let constraints' =
    List.map (fun ineq -> Lin_ineq.substitute ineq i sub) (constraints cle)
  in
  construct constraints' (exprs cle)

let substitute_from cle from i sub =
  try
    construct (constraints cle)
      (Lin_expr_vec.substitute_from (exprs cle) from i sub)
  with Invalid_argument _ ->
    invalid_arg "Cle_vec.substitute_from: variable index must be >= 1"

(* PRINT *)
let to_string cle =
  let ineqs = constraints cle in
  let conds =
    match ineqs with
    | [] -> "{}"
    | _ ->
        let strs = List.map Lin_ineq.to_string ineqs in
        "{\n" ^ String.concat "\n" strs ^ "\n}"
  in
  conds ^ " ⊢ " ^ Lin_expr_vec.to_string (exprs cle)

let print cle = print_string (to_string cle)
