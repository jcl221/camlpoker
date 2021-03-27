(** The type of card suits. *)
type suit =
  | Hearts
  | Diamonds
  | Spades
  | Clubs

(** A type representing a card with a suit and numerical rank in the
    range 1..13. *)
type t = {
  suit : suit;
  rank : int;
}

(** [init_card suit rank] is the card with suit [suit] and rank [rank]. *)
val init_card : suit -> int -> t

(** [get_suit card] is the suit of card [card]. *)
val get_suit : t -> suit

(** [get_rank card] is the rank of card [card]. *)
val get_rank : t -> int

(** [string_of_card] is the string representation of card [card]. *)
val string_of_card : t -> string
