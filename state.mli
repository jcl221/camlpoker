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

(** [add_turn st] mutates state [st] accordingly to give each active player
    with stakes in the game a pending turn to perform an action. *)
val add_turns : t -> unit

(** [perform_turn st id cmd] mutates state [st] accordingly after player 
    with id [id] has had their turn and performed an action [cmd]. *)
val perform_turn : t -> string -> string -> unit

(** [has_forfeited st id] is whether the player with id [id] in state [st]
    has forfeited. 
    Raises: Not_found if there is no player listed w/ id [id] in state [st]. *)
val has_forfeited : t -> string -> bool

(** [player_info st id] is the string representation of a player with id [id]
    in state [st], displaying the player's id, the amount of chips they have, 
    and their bet. 
    Raises: Not_found if there is no player listed w/ id [id] in state [st]. *)
val player_info : t -> string -> string

(** [string_of_hand st id] is the string representation of the hand held by 
    player with id [id] in state [st]. 
    Raises: Not_found if there is no player listed w/ id [id] in state [st]. *)
val string_of_hand : t -> string -> string

(** [string_of_table st] is the string representation of the poker table 
        (i.e., the deck and community cards) in state [st]. *)
val string_of_table : t -> string

(** [player_hands st] is a list of pairs corresponding to players still
    in the game. Each such pair contains the player's id and their hand.
    Raises: Invalid_Hand if players have not yet been dealt cards. *)
val player_hands : t -> string * (Card.t * Card.t) list
