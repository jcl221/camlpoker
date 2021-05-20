type suit =
  | Hearts
  | Diamonds
  | Spades
  | Clubs

(** AF: The record {suit ; rank} is a card of suit [suit] and rank
    [rank]. RI: The value bound to rank must be in the range 2..14. *)
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
    | 14 -> "A" ^ " "
    | 11 -> "J" ^ " "
    | 12 -> "Q" ^ " "
    | 13 -> "K" ^ " "
    | x -> if x <> 10 then string_of_int x ^ " " else string_of_int x
  in
  let s =
    match card.suit with
    | Hearts -> "♥"
    | Diamonds -> "♦"
    | Spades -> "♠"
    | Clubs -> "♣"
  in
  "\n......\n|" ^ s ^ "   |\n| " ^ r ^ " |\n|   " ^ s ^ "|\n......"
