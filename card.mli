(** The type of card suits. *)
type suit =
  | Hearts
  | Diamonds
  | Spades
  | Clubs

(** A type representing a card with a suit and numerical rank in the
    range 1..13. *)
type t

val get_suit : t -> suit

val get_rank : t -> int
