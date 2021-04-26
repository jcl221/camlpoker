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

(** [player_info id] is the string representation of a player with id [id],
    displaying the player's id and the amount of chips they have.
    For example, the form is ["Name: "player.name", Chips: "player.stack] 
    Raises: Not_found if there is no player listed w/ id [id] in state [st]. *)
val player_info : player -> string -> string
