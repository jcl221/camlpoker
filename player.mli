(*The abstract type of values representing a player*)
type player

(*The type of player's hand*)
type hand = Card.t list

(* Initializes the player type *)
val init : string -> int -> player

(* [hand_to_string hand] Converts a [hand] to a string *)
val hand_to_string : hand -> string
