type hand_type = Card.t * Card.t

type player = {
  name : string;
  hand : hand_type;
  stack : int;
  last_decision : string option;
  folded : bool;
  is_AI : bool;
}

let player_init (n : string) (c : Table.deck) =
  {
    name = n;
    hand = Table.deal_one_hand c;
    stack = 200;
    last_decision = None;
    folded = false;
    is_AI = (if n = "AI" then true else false);
  }

let string_of_hand player =
  match player.hand with
  | c1, c2 ->
      let c1_string = Card.string_of_card c1 in
      let c2_string = Card.string_of_card c2 in
      "( " ^ c1_string ^ ", " ^ c2_string ^ " )"
