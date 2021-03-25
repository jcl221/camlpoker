exception Invalid_Deck
exception Empty_Deck

type deck = Card.t list

(** AF: [table] is a tuple representing all cards in the deck and the commmunity
    cards on the board.
    RI: There can be no more than 52 cards in the deck and no duplicates*)
type table = deck * Card.t list option

(** [arr_mutator_help arr i] adds 1 to the element at index i in Array arr.*)
let arr_mutater_help arr i =
  arr.(i) <- arr.(i) + 1

(** [has_4_of_rank c arr] is true if the c has 4 cards of each rank.
    [arr] is a list of 13 elements that tracks how many elements have been counted
    for each rank.*)
let rec has_4_of_rank c arr =
  match c with
  | [] -> Array.fold_left (fun a x -> a && (x = 4)) true arr
  | {suit = _ ; rank = r} :: t -> begin
    let index = r - 1 in
    let _ = arr_mutator_help arr index in
    has_4_of_rank t
  end

  (** [has_13_of_suit c arr] is true if c has 13 cards of each suit.
      [arr] is a list of 4 elements that tracks how many elements have been counted
      for each suit.*)
let rec has_13_of_suit c arr =
  match c with
  | [] -> arr.(0) = 13 && arr.(1) = 13 && arr.(2) = 13 && arr.(3) = 13
  | {suit = s ; rank = _} :: t -> begin
    match (s: suit) with
    | Hearts -> begin
      let _ = arr_mutator_help arr 0 in
      has_13_of_suit c arr
    end
    | Diamonds -> begin
      let _ = arr_mutator_help arr 1 in
      has_13_of_suit c arr
    end
    | Spades -> begin
      let _ = arr_mutator_help arr 2 in
      has_13_of_suit c arr
    end
    | Clubs -> begin
      let _ = arr_mutator_help arr 3 in
      has_13_of_suit c arr
    end
  end

let valid_start c =
  if List.length c = 52 &&
    has_4_of_rank c (Array.make 13 0) &&
    has_13_of_suit c (Array.make 4 0)
  then c
  else raise Invalid_Deck 

(** [shuffle_help shuff current] is the randomly shuffled deck that results from the
    original deck current.*)
let rec shuffle_help shuff current =
  if (List.length shuff = 52) && (List.length current = 0) then shuff
  else begin
    Random.self_init ();
    let ind = Random.int (List.length current) in
    let select = List.nth current ind in
    let updated_current = List.filter (fun x -> x <> select) current in
    shuffle_help (updated_current :: shuff) updated_current
  end

let shuffle c =
  shuffle_help [] c


(** [init_deck] is the starting 52 card deck used in Texas Hold-em *)
val init_deck : unit -> deck

(** [new_card c] takes the first card out of the deck of table c and
    puts in the optional community card list.*)
val new_card : table -> table