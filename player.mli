(*The type of player's hand*)
type hand_type = Card.t list

(*The abstract type of values representing a player*)
type player = {
    name : string;
    hand : hand_type;
    stack : int;
    last_decision : string option;
    folded : bool;
    is_AI : bool;
  }


(* [player_init] initializes the [player] type*)
val player_init : string -> hand_type -> player

(* [hand_to_string hand] Converts a given players [player] hand to a string *)
val hand_to_string : player -> string
