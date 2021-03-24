type suit =
  | Hearts
  | Diamonds
  | Spades
  | Club

(** AF: The record { suit = [s]; rank = [r] } is a card of suit [s] and
    rank [r]. 
    RI: The value bound to rank must be in the range 1..13. *)
type t = {
  suit : suit;
  rank : int;
}

let get_suit card = card.suit

let get_rank card = card.rank
