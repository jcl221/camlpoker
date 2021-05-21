(** AF: A record \{ name; hand; stack; last_decision; folded; is_AI \}
    is the player at a poker table with name [name], a poker hand of
    [hand], an amount of chips [stack], and whose most recent action is
    [last_decision]. Whether the player has folded and whether they are
    an AI is given by [folded] and [is_AI].

    RI: The two cards in [hand] are not of the same value and rank.
    [stack] is a nonnegative value. *)
type player = {
  name : string;
  hand : Card.t * Card.t;
  stack : int;
  last_decision : string option;
  folded : bool;
  is_AI : bool;
}

let player_init chips table name =
  {
    name;
    hand = Table.deal_hand table;
    stack = chips;
    last_decision = None;
    folded = false;
    is_AI = (if name = "Dummy" then true else false);
  }

let string_of_hand player =
  match player.hand with
  | c1, c2 ->
      let c1_string = Card.string_of_card c1 in
      let c2_string = Card.string_of_card c2 in
      "( " ^ c1_string ^ ", " ^ c2_string ^ " )"

let player_info p =
  "Name: " ^ p.name ^ " (Chips: " ^ string_of_int p.stack ^ ")"

let reset_player pl = { pl with last_decision = None; folded = false }
