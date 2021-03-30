(** a [deck] is a list of cards where no two cards are identical *)
type deck = Card.t list

(** a [table] is a combination of the deck and optional community cards.
    There can be no community cards at the beginning of each hand.*)
type table = deck * Card.t list option

(** [valid_start c] is c if c is a valid starting deck (ie. all 52
    starting cards, 13 of each suit, 4 of each rank).*)
val valid_start : deck -> deck

(** [shuffle c] is a randomized copy of deck c*)
val shuffle : deck -> deck

(** [init_deck ()] is the starting 52 card deck used in Texas Hold-em *)
val init_deck : unit -> deck

(** [init_table ()] is a new table with the starting 52-card deck and no
    community cards. *)
val init_table : unit -> table

(** [create c board] is the table with deck [deck] and community cards
    listed in [board]. *)
val create : deck -> Card.t list option -> table

(** [new_card c] takes the first card out of the deck of table c and
    puts in the optional list *)
val new_card : table -> table

(** [deal_one_hand c] takes the first two cards out of the deck and
    returns them representing giving the two cards to a player.*)
val deal_one_hand : deck -> Card.t * Card.t

exception Invalid_Deck

exception Empty_Deck
