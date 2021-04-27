type player = {
  name : string;
  hand : Card.t * Card.t;
  stack : int;
  last_decision : string option;
  folded : bool;
  is_AI : bool;
}

let player_init table (n : string) =
  {
    name = n;
    hand = Table.deal_hand table;
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

let player_info player =
  "Name: " ^ player.name ^ ", Chips: " ^ string_of_int player.stack

let reset_player pl =
  {pl with last_decision = None; folded = false}
