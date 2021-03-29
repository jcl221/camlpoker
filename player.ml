type hand_type = Card.t list

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
    is_AI = if n = "AI" then true else false;
  }

(*This function is from A2 (was provided to us). Not using it right now, ignore it*)
let pp_string s = "\"" ^ s ^ "\""

(*This function is from A2 (was provided to us). Not using it right now, ignore it*)
let pp_list pp_elt lst =
  let pp_elts lst =
    let rec loop n acc = function
      | [] -> acc
      | [ h ] -> acc ^ pp_elt h
      | h1 :: (h2 :: t as t') ->
          if n = 100 then acc ^ "..." (* stop printing long list *)
          else loop (n + 1) (acc ^ pp_elt h1 ^ "; ") t'
    in
    loop 0 "" lst
  in
  "[" ^ pp_elts lst ^ "]"

let rec string_helper lst acc sep count =
  match lst with
  | [] -> acc
  | h :: t -> begin
    if count = 0
    then string_helper t (Card.string_of_card h) sep (count + 1)
    else string_helper t (acc ^ sep ^ Card.string_of_card h) sep (count + 1)
  end

let hand_to_string player =
  let hand_to_print = player.hand in
  string_helper hand_to_print "" " and " 0
