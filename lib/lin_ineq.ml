type rel = LessThan | LessEqual | GreaterThan | GreaterEqual
type t = Lin_expr.t * Lin_expr.t * rel

let compare val1 val2 rel =
  match rel with
  | LessThan -> val1 < val2
  | LessEqual -> val1 <= val2
  | GreaterThan -> val1 > val2
  | GreaterEqual -> val1 >= val2

let is_satisfied point (le1, le2, rel) =
  let val1 = Lin_expr.eval le1 point and val2 = Lin_expr.eval le2 point in
  compare val1 val2 rel

let rec find_unsatisfied point = function
  | [] -> None
  | ineq :: ineqs ->
      if not (is_satisfied point ineq) then Some ineq
      else find_unsatisfied point ineqs

let negate (le1, le2, rel) =
  let rel =
    match rel with
    | LessThan -> GreaterEqual
    | LessEqual -> GreaterThan
    | GreaterThan -> LessEqual
    | GreaterEqual -> LessThan
  in
  (le1, le2, rel)
