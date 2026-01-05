type t = Q.t list

(** [from_list lst] creates a value of type [t] from a list of rationals [lst].
    A 1 is prepended to the list so that the resulting vector can be
    scalar-multiplied with the coefficients of a linear expression of the form
    [[q_n; ...; q_1; q_0]]. *)
let from_list lst = List.rev (Q.one :: lst)
