type t = Lin_ineq.t list

let of_list ineqs = ineqs
let singleton ineq = [ ineq ]
let empty = []
let add t ineq = ineq :: t
let union t1 t2 = t1 @ t2

let is_satisfied t point =
  List.for_all (fun ineq -> Lin_ineq.is_satisfied ineq point) t

let find_unsatisfied t point =
  List.find_opt (fun ineq -> not (Lin_ineq.is_satisfied ineq point)) t

let substitute t i expr =
  List.map (fun ineq -> Lin_ineq.substitute ineq i expr) t

let substitute_many t indices subs =
  List.map
    (fun ineq ->
      let rec aux ineq j =
        if j >= Array.length indices then ineq
        else aux (Lin_ineq.substitute ineq indices.(j) subs.(j)) (j + 1)
      in
      aux ineq 0)
    t
