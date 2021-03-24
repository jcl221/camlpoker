(** a [deck] is a list of cards where no two cards are identical *)
type deck = Card.card list

(** a [table] is a combination of the deck and optional community cards.
    There can be no community cards at the beginning of each hand.*)
type table = deck * Card.card list option

(** [valid_start c] is c if c is a valid starting deck (ie. all 52 starting
    cards, 13 of each suit, 4 of each rank).*)
val valid_start :  deck -> deck

(** [shuffle c] is a randomized copy of deck c*)
val shuffle : deck -> deck

(** [init_deck] is the starting 52 card deck used in Texas Hold-em *)
val init_deck : unit -> deck

(** [new_card c] takes the first card out of the deck of table c and puts in 
    the optional list *)
val new_card : table -> table