(** The abstract type of values representing the game state. *)
type t

type stage =
  | Preflop
  | Midgame
  | Showdown

(** Raised when a player tries to play with an empty hand. *)
exception Empty_Hand

(** [init_state ids] is the initial state from which a poker match
    between players (listed by their ids in [ids]) begins. In this
    state, the players have yet to place any bets, and no cards have
    been dealt. *)
val init_state : string list -> t

(** [deal_center count st] is the new state starting from state [st] after 
    dealing a community card in the center of the table [count] number 
    of times. *)
val deal_center : int -> t -> t

(** [fold id st] is the new state [st] after player with id [id] folds. *)
val fold : string -> t -> t

(** [bet id amt st] is the new state [st] after player
    with id [id] decides to place a bet of amount [amt]. *)
val bet : string -> int -> t -> t

(** [showdown st] is the new state [st] after all active
    players reveal their hands and a winner is determined. The amount in the
    pot goes to the winning player and except for the players, the state is
    returned to its initial state.*)
val showdown : t -> t

(** [stage_of_game st] is the stage of the game corresponding to 
    game state [st]. *)
val stage_of_game : t -> stage

(** [ready_players st] is a list of the names of players who still have 
    stakes in the game (have not folded) in state [st]. *)
val ready_players : t -> string list

(** [active_bet st] is the active bet amount for the current round in 
    state [st]. *)
val active_bet : t -> int

(** [get_player st id] is the active player with id [id] for game state
    [st].
    Raises: Not_found if there is no player with id [id] in the given 
    state.*)
val get_player : string -> t -> Player.player

(** [player_hands st] is a list of pairs corresponding to players still
    in the game. Each such pair contains the player's id and their hand.
    Raises: Invalid_Hand if players have not yet been dealt cards. *)
val player_hands : t -> (string * (Card.t * Card.t)) list

(** [print_state st main_user] prints a string representation of state [st].
    This includes printing the community cards, player information, and 
    player hands for [st]. All hands except for that of the player 
    with name [main_user] is obscured. *)
val print_state : t -> string -> unit
