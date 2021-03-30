open OUnit2
open Player
open Table
open Card

let pp_string s = "\"" ^ s ^ "\""
let new_deck = init_deck ()
let shuffled_deck = shuffle new_deck

let hand1 = [{suit = Spades; rank = 10}; {suit = Hearts; rank = 12}]
let hand2 = [{suit = Clubs; rank = 14}; {suit = Diamonds; rank = 8}]
let string_of_hand1 = "10 of Spades and Queen of Hearts"
let string_of_hand2 = "Ace of Clubs and 8 of Diamonds"
let player1 = player_init "p1" hand1
let player2 = player_init "AI" hand2

let player_string_help test_name player expected =
  test_name >:: fun _ ->
    assert_equal (hand_to_string player) expected ~printer:(pp_string)

let player_help name player_value expected =
  name >:: fun _ ->
    assert_equal player_value expected
let player_tests = 
  [
    player_string_help "p1 hand to string" player1 string_of_hand1;
    player_string_help "p2 hand to string" player2 string_of_hand2;
    player_help "stack check" player1.stack 200;
    player_help "name check" player1.name "p1" ;
    player_help "last decision check" player1.last_decision None;
    player_help "folded check" player1.folded false;
    player_help "is AI check player 1" player1.is_AI false;
    player_help "is AI check player 2" player2.is_AI true;
  ]

let init_card_test name suit rank expected_output =
  name >:: fun ctxt ->
    assert_equal expected_output (init_card suit rank)
let get_suit_test name card expected_output =
  name >:: fun ctxt -> assert_equal expected_output (get_suit card)
  
let get_rank_test name card expected_output =
  name >:: fun ctxt -> assert_equal expected_output (get_rank card)
  
let string_of_card_test name card expected_output =
  name >:: fun ctxt ->
    assert_equal expected_output (string_of_card card)
  
let card_mod_tests =
  [
    init_card_test "First init test" Hearts 6
      { suit = Hearts; rank = 6 };
    init_card_test "Second init test" Spades 8
      { suit = Spades; rank = 8 };
    get_suit_test "First get_suit test" { suit = Diamonds; rank = 3 } Diamonds;
    get_suit_test "Second get_suit test" { suit = Clubs; rank = 11 } Clubs;
    get_rank_test "First get_rank test" { suit = Hearts; rank = 6 } 6;
    get_rank_test "Second get_rank test" { suit = Spades; rank = 9 } 9;
    string_of_card_test "First string_of_card test" { suit = Spades; rank = 7 }
      "7 of Spades";
    string_of_card_test "Second string_of_card test"
      { suit = Diamonds; rank = 3 } "3 of Diamonds";
    ]

(** [deck_size_test name deck expected] is the OUnit test named [name],
    asserting that the size of deck [deck] equals [expected]. *)
let deck_size_test name deck expected =
  name >:: fun _ ->
  assert_equal expected (List.length deck) ~printer:string_of_int

(** [contains_card card deck] is whether the deck [deck] contains card
    [card]. *)
let contains_card ?print_missing:(pm = false) card deck =
  match List.find_opt (fun c -> c = card) deck with
  | Some x -> true
  | None ->
      if pm then print_endline ("\nmissing" ^ string_of_card card);
      false

(** [full_deck_test name deck] is the OUnit test named [name], asserting
    that [deck] is a full deck (i.e. contains all ranks 1..13 for each
    suit). Note that a deck with more cards than those required of a
    full deck can pass this test. *)
let full_deck_test name deck =
  name >:: fun _ ->
  let suits = [| Hearts; Diamonds; Spades; Clubs |] in
  for s = 0 to 3 do
    for r = 2 to 14 do
      let card = Card.init_card suits.(s) r in
      assert (contains_card card deck ~print_missing:true)
    done
  done

(** [raises_exn_test name f exn] is an OUnit test named [name] asserting
    that calling [f ()] raises exception [exn]. *)
let raise_exn_test name exn f = name >:: fun _ -> assert_raises exn f

(** [valid_start_test name deck] is the OUnit test named [name],
    asserting the equality of [deck] and [valid_start deck]. *)
let valid_start_test name deck =
  name >:: fun _ -> assert_equal deck (valid_start deck)

(** [new_card_test name table] is the OUnit test named [name], asserting
    that [new_table table] is a table:

    1) with a deck that does not contain the most recent community card
    2) whose most recent community card was in the table to draw from,
    [table] *)
let new_card_test name table =
  name >:: fun _ ->
  let new_table = new_card table in
  let drawn =
    match snd new_table with
    | Some (h :: _) -> h
    | Some [] | None -> failwith "No card drawn"
  in
  let old_deck = fst table in
  let new_deck = fst new_table in
  assert (contains_card drawn old_deck);
  assert (not (contains_card drawn new_deck))

(* Test decks *)
let start_deck = init_deck ()

let shuffled_deck = shuffle start_deck

let oversized = { suit = Hearts; rank = 1 } :: start_deck

let one_card_deck = [ { suit = Hearts; rank = 1 } ]

(* Test tables *)
let empty_table = init_table [] None

let start_table = init_table start_deck None

let one_community_card = new_card start_table

let table_tests =
  [
    deck_size_test "[init_deck ()] has 52 elements" start_deck 52;
    full_deck_test "[init_deck ()] is a full deck" start_deck;
    deck_size_test "shuffling preserves deck size" shuffled_deck 52;
    full_deck_test "shuffling preserves elements of a deck"
      shuffled_deck;
    raise_exn_test "oversized deck is an invalid starting deck"
      Invalid_Deck (fun _ -> valid_start oversized);
    raise_exn_test "deck with one card is an invalid starting deck"
      Invalid_Deck (fun _ -> valid_start one_card_deck);
    
    valid_start_test "A full unshuffled deck with 52 cards is valid."
      start_deck;
    valid_start_test "A full shuffled deck with 52 cards is valid"
      shuffled_deck;
    
    deck_size_test
      "drawing a card results in table with deck size decreased by 1"
      (fst one_community_card)
      51;
    new_card_test "drawing a card removes it from deck" start_table;
    raise_exn_test
      "drawing a card from table with empty deck raises Invalid_Deck"
      Invalid_Deck (fun _ -> new_card empty_table);
  ]
let suite =
  "test suite for MS1"
    >::: List.flatten [ player_tests ; card_mod_tests ; table_tests]
  
let _ = run_test_tt_main suite
