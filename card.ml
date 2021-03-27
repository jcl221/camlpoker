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

let get_suit card = card.suit

let get_rank card = card.rank

let string_of_card card =
  let s =
    match card.suit with
    | Hearts -> "Hearts"
    | Diamonds -> "Diamonds"
    | Spades -> "Spades"
    | Clubs -> "Clubs"
  in
  let r = string_of_int card.rank in
  r ^ " of " ^ s
