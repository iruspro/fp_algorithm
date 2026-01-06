type t = Q.t list (* [r_n; ...; r_1] *)

let from_list : Q.t list -> t = List.rev

(* Tests *)
(* from_list *)
let%test "from_list" =
  from_list [ Q.zero; Q.one ] = [ Q.one; Q.zero ]
  && List.length (from_list (List.init 5 (fun i -> Q.of_int i))) = 5
