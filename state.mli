(** The abstract type of values representing the game state. *)
type t

(** Raised when a player tries to play with an empty hand. *)
exception Empty_Hand

(** [init_state ids] is the initial state from which a poker match
    between players (listed by their ids in [ids]) begins. In this
    state, the players have yet to place any bets, and no cards have
    been dealt. *)
val init_state : string list -> t

(** [deal st] mutates state [st] accordingly after the player hands and
    flop are dealt out at the beginning of a match. *)
val deal : t -> unit

(** [fold st player_id] mutates state [st] accordingly after player with
    id [id] folds. *)
val fold : t -> string -> unit

(** [bet st player_id amt] mutates state [st] accordingly after player
    with id [id] decides to place a bet of amount [amt]. *)
val bet : t -> string -> int -> unit

(** [showdown st] mutates state [st] accordingly after all active
    players reveal their hands and a winner is determined. *)
val showdown : t -> unit

(** [get_hands st] is a list of pairs corresponding to players still in
    the game. Each such pair contains the player's id and their hand.
    Raises: Invalid_Hand if players have not yet been dealt cards. *)
val get_hands : t -> (string * Card.t list) list

(** [get_winner st] is the id of the winning player. *)
val get_winner : t -> string option

(** [player_hands st] is a list of pairs containing each active player's
    id and hand during the game state [st]. *)
val player_hands : t -> string * (Card.t * Card.t) list
