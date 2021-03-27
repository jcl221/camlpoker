(*The abstract type of values representing a player*)
type player

(*The type of player's hand*)
type hand_type = Card.t list

(* [player_init] initializes the [player] type*)
val player_init : string -> int -> player

(* [hand_to_string hand] Converts a given players [player] hand to a string *)
val hand_to_string : player -> string
