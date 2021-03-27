type suit =
  | Hearts
  | Diamonds
  | Spades
  | Clubs

(** AF: The record { suit = [s]; rank = [r] } is a card of suit [s] and
    rank [r]. 
    RI: The value bound to rank must be in the range 1..13. *)
type t = {
  suit : suit;
  rank : int;
}

let init_card suit rank = { suit; rank }

let get_suit card = card.suit

let get_rank card = card.rank
