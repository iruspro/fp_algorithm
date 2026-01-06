type rel = LessThan | LessEqual | GreaterThan | GreaterEqual

type n_type =
  | WithoutLast
  | LastGreaterThan
  | LastGreaterEqual
  | LastLessThan
  | LastLessEqual

type t = Lin_expr.t * Lin_expr.t * rel * n_type

let change_rel = function
  | LessThan -> GreaterThan
  | LessEqual -> GreaterEqual
  | GreaterThan -> LessThan
  | GreaterEqual -> GreaterThan

let rel_to_n_type = function
  | LessThan -> LastLessThan
  | LessEqual -> LastLessEqual
  | GreaterThan -> LastGreaterThan
  | GreaterEqual -> LastGreaterEqual

let construct (dim : int) (expr1 : Lin_expr.t) (expr2 : Lin_expr.t) (rel : rel)
    : t =
  let one = Q.one :: Lin_expr.zero (pred dim) in
  match (expr1, expr2) with
  | [ _ ], [ _ ] -> (expr1, expr2, rel, WithoutLast)
  | q_n :: qs, r_n :: rs when (q_n = Q.zero && r_n = Q.zero) || q_n = r_n ->
      (Q.zero :: qs, Q.zero :: rs, rel, WithoutLast)
  | q_n :: qs, r_n :: rs when q_n > r_n ->
      let expr1 = Lin_expr.mul_by (Q.sub q_n r_n) one
      and expr2 =
        Q.zero :: Lin_expr.add rs (Lin_expr.mul_by (Q.of_int (-1)) qs)
      in
      (expr1, expr2, rel, rel_to_n_type rel)
  | q_n :: qs, r_n :: rs (* q_n < r_n case *) ->
      let expr1 = Lin_expr.mul_by (Q.sub r_n q_n) one
      and expr2 = Q.zero :: Lin_expr.add qs (Lin_expr.mul_by (Q.of_int (-1)) rs)
      and rel = change_rel rel in
      (expr1, expr2, rel, rel_to_n_type rel)
  | _ -> failwith "A linear expression must have at least one coefficient"

let compare val1 val2 rel =
  match rel with
  | LessThan -> val1 < val2
  | LessEqual -> val1 <= val2
  | GreaterThan -> val1 > val2
  | GreaterEqual -> val1 >= val2

let is_satisfied point (le1, le2, rel, _) =
  let val1 = Lin_expr.eval le1 point and val2 = Lin_expr.eval le2 point in
  compare val1 val2 rel

let rec find_unsatisfied point = function
  | [] -> None
  | ineq :: ineqs ->
      if not (is_satisfied point ineq) then Some ineq
      else find_unsatisfied point ineqs

let negate (expr1, expr2, rel, n_type) =
  let rel =
    match rel with
    | LessThan -> GreaterEqual
    | LessEqual -> GreaterThan
    | GreaterThan -> LessEqual
    | GreaterEqual -> LessThan
  and n_type =
    match n_type with
    | WithoutLast -> WithoutLast
    | LastGreaterThan -> LastLessEqual
    | LastGreaterEqual -> LastLessThan
    | LastLessThan -> LastGreaterEqual
    | LastLessEqual -> LastGreaterThan
  in
  (expr1, expr2, rel, n_type)

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
                  acc && is_satisfied point (construct dim expr1 expr2 rel)
              | LessThan | LessEqual ->
                  acc || is_satisfied point (construct dim expr1 expr2 rel)
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
  let dim = 2 in
  let expr = List.init (succ dim) (fun _ -> Q.of_int 2)
  and zero = Lin_expr.zero dim
  and point = [ Q.of_int (-1); Q.of_int 0 ] in
  is_satisfied point (construct dim expr zero LessEqual)
  && is_satisfied point (construct dim expr zero GreaterEqual)
  && (not (is_satisfied point (construct dim expr zero LessThan)))
  && not (is_satisfied point (construct dim expr zero GreaterThan))

(* find_unsatisfied *)
let%test "find_unsatisfied success" =
  let dim = 2 in
  let zero = Lin_expr.zero dim in
  let ineqs =
    [
      construct dim
        (List.init (succ dim) (fun _ -> Q.of_int 2))
        zero GreaterThan;
      construct dim
        (List.init (succ dim) (fun _ -> Q.of_int 3))
        zero GreaterThan;
      construct dim
        (List.init (succ dim) (fun _ -> Q.of_int (-2)))
        zero GreaterThan;
      construct dim
        (List.init (succ dim) (fun _ -> Q.of_int 3))
        zero GreaterThan;
      construct dim
        (List.init (succ dim) (fun _ -> Q.of_int (-1)))
        zero GreaterThan;
    ]
  and point = [ Q.of_int 1; Q.of_int 1 ] in
  let res = find_unsatisfied point ineqs in
  match res with None -> false | Some _ -> true

let%test "find_unsatisfied fail" =
  let dim = 2 in
  let zero = Lin_expr.zero dim in
  let ineqs =
    [
      construct dim
        (List.init (succ dim) (fun _ -> Q.of_int 2))
        zero GreaterThan;
      construct dim
        (List.init (succ dim) (fun _ -> Q.of_int 3))
        zero GreaterThan;
      construct dim
        (List.init (succ dim) (fun _ -> Q.of_int (-2)))
        zero LessThan;
      construct dim
        (List.init (succ dim) (fun _ -> Q.of_int 3))
        zero GreaterThan;
      construct dim
        (List.init (succ dim) (fun _ -> Q.of_int (-1)))
        zero LessThan;
    ]
  and point = [ Q.of_int 1; Q.of_int 1 ] in
  let res = find_unsatisfied point ineqs in
  match res with None -> true | Some _ -> false

(* negate *)
let%test "negate LessThan -> GreaterEqual" =
  let expr1 = [ Q.one ] and expr2 = [ Q.zero ] in
  let input = construct 1 expr1 expr2 LessThan in
  let _, _, r, _ = negate input in
  r = GreaterEqual

let%test "negate LessEqual -> GreaterThan" =
  let expr1 = [ Q.one ] and expr2 = [ Q.zero ] in
  let input = construct 1 expr1 expr2 LessEqual in
  let _, _, r, _ = negate input in
  r = GreaterThan

let%test "negate GreaterThan -> LessEqual" =
  let expr1 = [ Q.one ] and expr2 = [ Q.zero ] in
  let input = construct 1 expr1 expr2 GreaterThan in
  let _, _, r, _ = negate input in
  r = LessEqual

let%test "negate GreaterEqual -> LessThan" =
  let expr1 = [ Q.one ] and expr2 = [ Q.zero ] in
  let input = construct 1 expr1 expr2 GreaterEqual in
  let _, _, r, _ = negate input in
  r = LessThan
