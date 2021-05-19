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
