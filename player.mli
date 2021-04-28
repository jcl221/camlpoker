(** The type of values representing a player. *)
type player = {
  name : string;
  hand : Card.t * Card.t;
  stack : int;
  last_decision : string option;
  folded : bool;
  is_AI : bool;
}

(** [player_init name chips table] creates and initializes a new player 
    with name [n] and an amount of chips [chips] at table [table]. *)
val player_init : int -> Table.table -> string -> player

(** [string_of_hand player] is the string representation of the hand 
    held by [p]. *)
val string_of_hand : player -> string

(** [player_info p] is the string representation of a player [p],
    displaying the player's id and the amount of chips they have. *)
val player_info : player -> string

(** [reset_player p] is the default version of player before a 
    new round starts. *)
val reset_player : player -> player
