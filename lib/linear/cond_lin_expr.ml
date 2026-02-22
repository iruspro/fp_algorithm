type t = { constraints : Lin_ineq.t list; expr : Lin_expr.t }

let construct constraints expr = { constraints; expr }
let constraints cle = cle.constraints
let expr cle = cle.expr

(* Functions *)
let with_expr cle expr = construct (constraints cle) expr

let flip_last cle =
  let constraints = List.map Lin_ineq.flip_last (constraints cle)
  and expr = Lin_expr.flip_last (expr cle) in
  construct constraints expr

(* Print *)
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
