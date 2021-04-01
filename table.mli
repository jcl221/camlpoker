(** a [deck] is a list of cards where no two cards are identical *)
type deck = Card.t list

(** a [table] is a combination of the deck and optional community cards.
    There can be no community cards at the beginning of each hand. *)
type table = {
  mutable deck : deck;
  mutable board : Card.t list option;
}

(** [valid_start c] is c if c is a valid starting deck (ie. all 52
    starting cards, 13 of each suit, 4 of each rank).*)
val assert_valid_start : deck -> unit

(** [init_deck ()] is the starting 52 card deck used in Texas Hold-em *)
val init_deck : unit -> deck

(** [shuffle deck] is a randomized copy of deck [deck]. 
    Requires: [deck] is a valid starting deck with 52 cards and 
    containing all ranks of each suit. *)
val shuffle : deck -> deck

(** [init_table ()] is a new table with the starting 52-card deck and no
    community cards. *)
val init_table : unit -> table

(** [create deck board] is the table with deck [deck] and community cards
    listed in [board]. *)
val create : deck -> Card.t list option -> table

(** [place_center table] takes the first card out of the deck of table [table] 
    and places it on the board (as a community card). *)
val place_center : table -> table

(** [deal_hand table] is a hand consisting of the top two cards of the deck 
    in [table]. Mutates [table] accordingly to remove these cards from 
    the deck. *)
val deal_hand : table -> Card.t * Card.t

exception Invalid_Deck

exception Empty_Deck
