type t = { exprs : Lin_expr.t array (* [| f_1; ...; f_n |] *) }

(* CONSTRUCTORS *)
let from_array exprs =
  if Array.length exprs = 0 then
    invalid_arg "Lin_expr_vec.from_array: array must be non-empty"
  else { exprs }

let const consts =
  if Array.length consts = 0 then
    invalid_arg "Lin_expr_vec.const: array must be non-empty"
  else { exprs = Array.map Lin_expr.const consts }

(* GETTERS *)
let dim le_vec =
  let arr = Array.map Lin_expr.dim le_vec.exprs in
  Array.fold_left max arr.(0) arr

let codim le_vec = Array.length le_vec.exprs

let expr le_vec k =
  if k < 1 || k > Array.length le_vec.exprs then
    invalid_arg "Lin_expr_vec.expr: index out of bounds"
  else le_vec.exprs.(k - 1)

(* OPERATORS *)
let with_expr le_vec k e =
  if k < 1 || k > Array.length le_vec.exprs then
    invalid_arg "Lin_expr_vec.with_expr: index out of bounds"
  else
    let exprs = Array.copy le_vec.exprs in
    exprs.(k - 1) <- e;
    { exprs }

let substitute_from le_vec from i sub =
  if i < 1 then
    invalid_arg "Lin_expr_vec.substitute_from: variable index must be >= 1"
  else
    let n = Array.length le_vec.exprs in
    let exprs = Array.copy le_vec.exprs in
    for j = max (from - 1) 0 to n - 1 do
      exprs.(j) <- Lin_expr.substitute exprs.(j) i sub
    done;
    { exprs }

let substitute le_vec k i sub =
  if k < 1 || k > Array.length le_vec.exprs then
    invalid_arg "Lin_expr_vec.substitute: component index out of bounds"
  else if i < 1 then
    invalid_arg "Lin_expr_vec.substitute: variable index must be >= 1"
  else
    let exprs = Array.copy le_vec.exprs in
    exprs.(k - 1) <- Lin_expr.substitute exprs.(k - 1) i sub;
    { exprs }

(* FUNCTIONS *)
let eval le_vec point =
  try Array.map (fun e -> Lin_expr.eval e point) le_vec.exprs
  with Invalid_argument _ ->
    invalid_arg
      "Lin_expr_vec.eval: dimension of vector exceeds dimension of point"

(* PRINT *)
let to_string le_vec =
  let parts = Array.to_list (Array.map Lin_expr.to_string le_vec.exprs) in
  "(" ^ String.concat ", " parts ^ ")"

let print le_vec = print_string (to_string le_vec)
