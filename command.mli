type t =
  | Bet of int
  | Check
  | Raise of int
  | Call
  | Fold
  | Invalid

(** [parse str] is the command corresponding to [str]. If [str] doesn't
    correspond to any valid poker action, [Invalid] is returned. *)
val parse : string -> t

(** [string_of_cmd cmd] is the string representation of command [cmd]. *)
val string_of_cmd : t -> string
