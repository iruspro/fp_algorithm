val int_to_subscript : int -> string
(** [int_to_subscript n] returns a string of Unicode subscript digits
    representing the non-negative integer [n]. For example,
    [int_to_subscript 42] returns ["₄₂"].

    Raise [Invalid_argument] if [n < 0]. *)
