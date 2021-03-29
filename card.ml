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

let string_of_card card =
  let r =
    match card.rank with
    | 1 -> "Ace"
    | 11 -> "Jack"
    | 12 -> "Queen"
    | 13 -> "King"
    | x -> string_of_int x
  in
  let s =
    match card.suit with
    | Hearts -> "Hearts"
    | Diamonds -> "Diamonds"
    | Spades -> "Spades"
    | Clubs -> "Clubs"
  in
  r ^ " of " ^ s
