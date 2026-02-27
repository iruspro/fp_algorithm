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
  let ineqs = constraints cle and expr = expr cle in

  let count = List.length ineqs in
  let constraints =
    "{\n"
    ^ List.fold_left ( ^ ) ""
        (List.mapi
           (fun i ineq ->
             if i <> count - 1 then Lin_ineq.to_string ineq ^ "\n"
             else Lin_ineq.to_string ineq)
           ineqs)
    ^ "\n}"
  in
  constraints ^ " ⊢ " ^ Lin_expr.to_string expr

let print cle = print_string (to_string cle)
