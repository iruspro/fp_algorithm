type t = { constraints : Lin_ineq.t list; exprs : Lin_expr.t array }

(* CONSTRUCTORS *)
let construct constraints exprs = { constraints; exprs }

(* GETTERS *)
let constraints cle = cle.constraints
let exprs cle = cle.exprs
let expr cle k = cle.exprs.(k)

(* FUNCTIONS *)
let add_constraint cle ineq = construct (ineq :: constraints cle) (exprs cle)
let add_constraints cle ineqs = construct (constraints cle @ ineqs) (exprs cle)

let with_expr cle expr k =
  let es = Array.copy (exprs cle) in
  es.(k) <- expr;
  construct (constraints cle) es

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
  let ineqs = constraints cle and es = exprs cle in
  let conds =
    match ineqs with
    | [] -> "{}"
    | _ ->
        let strs = List.map Lin_ineq.to_string ineqs in
        "{\n" ^ String.concat "\n" strs ^ "\n}"
  in
  let exprs_str =
    "("
    ^ String.concat ", " (Array.to_list (Array.map Lin_expr.to_string es))
    ^ ")"
  in
  conds ^ " ⊢ " ^ exprs_str

let print cle = print_string (to_string cle)
