type t = Q.t list (* [r_n; ...; r_1] *)

let extend_dim (point : t) (v : Q.t) : t = v :: point
let from_array (arr : Q.t array) : t = List.rev (Array.to_list arr)
let as_list (point : t) : Q.t list = point

(* Tests *)
(* extend_dim *)
let%test "extend_dim" =
  let point = from_array [| Q.one; Q.one |] in
  extend_dim point (Q.of_int 2) = [ Q.of_int 2; Q.one; Q.one ]

(* from_array *)
let%test "from_array" =
  from_array [| Q.zero; Q.one |] = [ Q.one; Q.zero ]
  && List.length (from_array (Array.init 5 (fun i -> Q.of_int i))) = 5
