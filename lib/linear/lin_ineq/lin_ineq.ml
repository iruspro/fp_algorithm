type rel = Lt | Le | Gt | Ge
type t = { lhs : Lin_expr.t; rhs : Lin_expr.t; rel : rel; dim : int }

let neg_rel = function Lt -> Ge | Le -> Gt | Gt -> Le | Ge -> Lt

let compare val1 val2 = function
  | Lt -> Q.lt val1 val2
  | Le -> Q.leq val1 val2
  | Gt -> Q.gt val1 val2
  | Ge -> Q.geq val1 val2

(* CONSTRUCTORS *)
let construct lhs rhs rel =
  (* lhs □ rhs <=> 0 □ rhs - lhs: q_s x_s + ... + q_1 x_1 + q_0 <=> 
     -q_s x_s □ q_{s-1} x_{s-1} + ... q_1 x_1 + q_0 <=> 
      x_s □ -1/q_s (q_{s-1} x_{s-1} + ... q_1 x_1 + q_0) *)
  let rhs = Lin_expr.sub rhs lhs in
  let dim = Lin_expr.dim rhs and leading_coeff = Lin_expr.leading_coeff rhs in
  if dim = 0 then { lhs = Lin_expr.const Q.zero; rhs; rel; dim }
  else
    let x =
      Lin_expr.x dim
      (* x_s *)
    in
    let rhs = Lin_expr.sub rhs (Lin_expr.mul_by leading_coeff x) in
    (* Invariant: leading_coeff <> 0 if dim > 0 *)
    let rhs = Lin_expr.mul_by (Q.neg (Q.inv leading_coeff)) rhs
    and rel = if Q.geq (Q.neg leading_coeff) Q.zero then rel else neg_rel rel in
    { lhs = x; rhs; rel; dim }

(* GETTERS *)
let lhs ineq = ineq.lhs
let rhs ineq = ineq.rhs
let rel ineq = ineq.rel
let dim ineq = ineq.dim

(* OPERATORS *)
let negate ineq = construct (lhs ineq) (rhs ineq) (neg_rel (rel ineq))

let substitute ineq i expr =
  if i < 1 then invalid_arg "Lin_ineq.substitute: variable index must be >= 1"
  else if i > dim ineq then ineq
  else if i = dim ineq then
    (* Invariant: dim rhs < dim ineq if dim ineq >= 1 *)
    let lhs = Lin_expr.substitute (lhs ineq) i expr in
    construct lhs (rhs ineq) (rel ineq)
  else
    (* Invariant: lhs = x_{dim ineq} *)
    let rhs = Lin_expr.substitute (rhs ineq) i expr in
    construct (lhs ineq) rhs (rel ineq)

(* FUNCTIONS *)
let is_satisfied ineq point =
  let val1 = Lin_expr.eval (lhs ineq) point
  and val2 = Lin_expr.eval (rhs ineq) point in
  compare val1 val2 (rel ineq)

(* PRINT *)
let rel_to_string = function Lt -> "<" | Le -> "≤" | Gt -> ">" | Ge -> "≥"

let to_string ineq =
  Lin_expr.to_string (lhs ineq)
  ^ " "
  ^ rel_to_string (rel ineq)
  ^ " "
  ^ Lin_expr.to_string (rhs ineq)

let print ineq = print_string (to_string ineq)
