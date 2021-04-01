(*The abstract type of values representing a player*)
type player = {
  name : string;
  hand : Card.t * Card.t;
  stack : int;
  last_decision : string option;
  folded : bool;
  is_AI : bool;
}

(* [player_init] initializes the [player] type*)
val player_init : string -> Table.table -> player

(* [string_of_hand player] is the string representation of the hand 
    held by [player]. *)
val string_of_hand : player -> string
