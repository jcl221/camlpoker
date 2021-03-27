(*The abstract type of values representing a player*)
type player

(*The type of player's hand*)
type hand_type = Card.t list

(* Initializes the player type *)
val init : string -> int -> player

(* [hand_to_string hand] Converts a given players hand to a string *)
val hand_to_string : player -> string
