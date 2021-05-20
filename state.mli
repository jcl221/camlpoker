(** The abstract type of values representing the game state. *)
type t = {
  players : Player.player list;
  table : Table.table;
  active_bet : int;
  pot : int;
  ai_difficulty : string;
}

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

(** [deal_center count st] is the new state starting from state [st]
    after dealing a community card in the center of the table [count]
    number of times. Requires: [count] > 0. *)
val deal_center : int -> t -> t

(** [fold id st] is the new state [st] after player with id [id] folds. *)
val fold : string -> t -> t

(** [bet (name, current_bet) raise_to st] is the new state [st] after
    the player named [name], having already made a bet of [current_bet]
    in the current betting round, decides to raise the active bet to
    amount [raise_to].

    Requires: There exists a player in [st] with name [name]. [raise_to]
    is > [current_bet] and >= the active bet in [st]. *)
val bet : string * int -> int -> t -> t

(** [reset winners st] is the new game state after resetting the game
    for a new match from state [st]. A reset includes distributing the
    pot equally amongst players listed in [winners], clearing the
    current table, and redealing hands to all players. *)
val reset : string list -> t -> t

(** [showdown st] is the new state from [st] after all active players
    reveal their hands and a winner is determined. The amount in the pot
    goes to the winning player and except for the players, the state is
    returned to its initial state. *)
val showdown : t -> t

(** [print_state st main_user mask] prints a string representation of
    state [st]. This includes printing the community cards, player
    information, and player hands for [st]. If [mask] is set to true,
    all hands except for that of the player with name [main_user] are
    obscured. *)
val print_state : t -> string -> bool -> unit

(** [stage_of_game st] is the stage of the game corresponding to game
    state [st]. *)
val stage_of_game : t -> stage

(** [active_bet st] is the active bet amount for the current round in
    state [st]. *)
val active_bet : t -> int

(** [ready_players st] is a list of the names of players who still have
    stakes in the pot (have not folded) in state [st]. *)
val active_players : t -> string list

(** [get_player st id] is the active player with id [id] for game state
    [st]. Raises: Not_found if there is no player with id [id] in the
    given state. *)
val get_player : string -> t -> Player.player

(** [player_hands st] is a list of pairs corresponding to players still
    in the game. Each such pair contains the player's id and their hand.
    Raises: Invalid_Hand if players have not yet been dealt cards. *)
val player_hands : t -> (string * (Card.t * Card.t)) list

(** [compare_hands h1 h2] is [1] if [h1] is a better hand than [h2],
    [-1] if it is worse, and 0 if the hands are tied. *)
val compare_hands : Card.t list -> Card.t list -> int
