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

(* Tests *)

(* is_satisfied *)
let%test "is_satisfied random" =
  let result = ref true in
  let dims = [| 0; 1; 5; 10 |] in
  let _ =
    for i = 1 to pred (Array.length dims) do
      let dim = dims.(i) in
      let expr1 = List.init (succ dim) (fun _ -> Q.of_int 2)
      and expr2 = List.init (succ dim) (fun _ -> Q.of_int 1) in
      let n_cases = 10 in
      let points =
        List.init n_cases (fun _ ->
            List.init dim (fun _ -> Q.of_int (Random.full_int 100)))
      in
      let rec aux acc rel = function
        | [] -> acc
        | point :: points ->
            let acc =
              match rel with
              | GreaterThan | GreaterEqual ->
                  acc && is_satisfied point (expr1, expr2, rel)
              | LessThan | LessEqual ->
                  acc || is_satisfied point (expr1, expr2, rel)
            in
            aux acc rel points
      in
      result :=
        aux true GreaterThan points
        && aux true GreaterEqual points
        && (not (aux false LessEqual points))
        && not (aux false LessThan points)
    done
  in
  !result

let%test "is_satisfied equals" =
  let expr = List.init 3 (fun _ -> Q.of_int 2)
  and zero = Lin_expr.zero 2
  and point = [ Q.of_int (-1); Q.of_int 0 ] in
  is_satisfied point (expr, zero, LessEqual)
  && is_satisfied point (expr, zero, GreaterEqual)
  && (not (is_satisfied point (expr, zero, LessThan)))
  && not (is_satisfied point (expr, zero, GreaterThan))

(* find_unsatisfied *)
let%test "find_unsatisfied success" =
  let zero = Lin_expr.zero 2 in
  let ineqs =
    [
      (List.init 3 (fun _ -> Q.of_int 2), zero, GreaterThan);
      (List.init 3 (fun _ -> Q.of_int 3), zero, GreaterThan);
      (List.init 3 (fun _ -> Q.of_int (-2)), zero, GreaterThan);
      (List.init 3 (fun _ -> Q.of_int 3), zero, GreaterThan);
      (List.init 3 (fun _ -> Q.of_int (-1)), zero, GreaterThan);
    ]
  and point = [ Q.of_int 1; Q.of_int 1 ] in
  let res = find_unsatisfied point ineqs in
  match res with None -> false | Some _ -> true

let%test "find_unsatisfied fail" =
  let zero = Lin_expr.zero 2 in
  let ineqs =
    [
      (List.init 3 (fun _ -> Q.of_int 2), zero, GreaterThan);
      (List.init 3 (fun _ -> Q.of_int 3), zero, GreaterThan);
      (List.init 3 (fun _ -> Q.of_int (-2)), zero, LessThan);
      (List.init 3 (fun _ -> Q.of_int 3), zero, GreaterThan);
      (List.init 3 (fun _ -> Q.of_int (-1)), zero, LessThan);
    ]
  and point = [ Q.of_int 1; Q.of_int 1 ] in
  let res = find_unsatisfied point ineqs in
  match res with None -> true | Some _ -> false

(* negate *)
let%test "negate LessThan -> GreaterEqual" =
  let expr1 = [ Q.one ] and expr2 = [ Q.zero ] in
  let input = (expr1, expr2, LessThan) in
  let e1, e2, r = negate input in
  r = GreaterEqual && e1 = expr1 && e2 = expr2

let%test "negate LessEqual -> GreaterThan" =
  let expr1 = [ Q.one ] and expr2 = [ Q.zero ] in
  let input = (expr1, expr2, LessEqual) in
  let e1, e2, r = negate input in
  r = GreaterThan && e1 = expr1 && e2 = expr2

let%test "negate GreaterThan -> LessEqual" =
  let expr1 = [ Q.one ] and expr2 = [ Q.zero ] in
  let input = (expr1, expr2, GreaterThan) in
  let e1, e2, r = negate input in
  r = LessEqual && e1 = expr1 && e2 = expr2

let%test "negate GreaterEqual -> LessThan" =
  let expr1 = [ Q.one ] and expr2 = [ Q.zero ] in
  let input = (expr1, expr2, GreaterEqual) in
  let e1, e2, r = negate input in
  r = LessThan && e1 = expr1 && e2 = expr2
