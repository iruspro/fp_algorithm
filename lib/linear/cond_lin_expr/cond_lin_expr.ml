type t = { constraints : Lin_ineq.t list; expr : Lin_expr.t }

(* CONSTRUCTORS *)
let construct constraints expr = { constraints; expr }

(* GETTERS *)
let constraints cle = cle.constraints
let expr cle = cle.expr

(* FUNCTIONS *)
let add_constraint cle ineq = construct (ineq :: constraints cle) (expr cle)
let add_constraints cle ineqs = construct (constraints cle @ ineqs) (expr cle)
let with_expr cle expr = construct (constraints cle) expr

(* PRINT *)
let to_string cle =
  let ineqs = constraints cle and e = expr cle in
  let conds =
    match ineqs with
    | [] -> "{}"
    | _ ->
        let strs = List.map Lin_ineq.to_string ineqs in
        "{\n" ^ String.concat "\n" strs ^ "\n}"
  in
  conds ^ " ⊢ " ^ Lin_expr.to_string e

let print cle = print_string (to_string cle)
