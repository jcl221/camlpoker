(****************************************************************)
(** Test Plan *)

(** The testing of modules Card, Player, and Table largely consisted of
    OUnit black-box tests, allowing us to ensure their adherence to
    their specifications before integrating them in larger modules such
    as State. The Command module was tested via glass-box tests that
    ensured edge cases in user input could be parsed correctly.

    The State module was tested using a combination of black-box tests,
    which were primarily used for simple getters/setters, glass-box
    testing of certain state transition functions (e.g., bet, fold,
    all-in, reset), and manual testing through the command-line.

    This manual component, in particular, was crucial in verifying that
    state transitions were properly integrated in Main to facilitate
    correct progression of the game. After each step in development, we
    frequently noted down possible input/command combinations and tested
    them against their expected results through the UI display. Later
    introduction of an AI opponent was also tested similarly by testing
    possible combinations of player input and AI responses.

    This method provided us reliable verification of core functionality
    in our base modules, while also allowing us to account for the many
    different edge cases and nuances that could arise when actually
    playing a poker match. *)

(****************************************************************)

open OUnit2
open Card
open Player
open Table
open State
open Command

let pp_string s = "\"" ^ s ^ "\""

let new_deck = init_deck ()

let shuffled_deck = shuffle new_deck

let hand1 = ({ suit = Spades; rank = 10 }, { suit = Hearts; rank = 12 })

let hand2 = ({ suit = Clubs; rank = 14 }, { suit = Diamonds; rank = 8 })

let string_of_hand1 =
  (*"( 10 of Spades, Queen of Hearts )"*)
  "( \n\
   ......\n\
   |♠   |\n\
   | 10 |\n\
   |   ♠|\n\
   ......, \n\
   ......\n\
   |♥   |\n\
   | Q  |\n\
   |   ♥|\n\
   ...... )"

let string_of_hand2 =
  (*"( Ace of Clubs, 8 of Diamonds )"*)
  "( \n\
   ......\n\
   |♣   |\n\
   | A  |\n\
   |   ♣|\n\
   ......, \n\
   ......\n\
   |♦   |\n\
   | 8  |\n\
   |   ♦|\n\
   ...... )"

let player1 =
  {
    name = "p1";
    hand = hand1;
    stack = 200;
    last_decision = None;
    folded = false;
    is_AI = false;
  }

let player2 =
  {
    name = "AI";
    hand = hand2;
    stack = 0;
    last_decision = None;
    folded = false;
    is_AI = true;
  }

let player3 =
  {
    name = "Clarkson";
    hand = hand2;
    stack = 133;
    last_decision = Some "Call";
    folded = false;
    is_AI = true;
  }

let player4 =
  {
    name = "James";
    hand = hand2;
    stack = 33;
    last_decision = Some "Fold";
    folded = true;
    is_AI = false;
  }

let player_string_help test_name player expected =
  test_name >:: fun _ ->
  assert_equal (string_of_hand player) expected ~printer:pp_string

let player_help name player_value expected =
  name >:: fun _ -> assert_equal player_value expected

let player_tests =
  [
    player_string_help "p1 hand to string" player1 string_of_hand1;
    player_string_help "p2 hand to string" player2 string_of_hand2;
    player_help "player1 stack check" player1.stack 200;
    player_help "player2 stack check" player2.stack 0;
    player_help "name check" player1.name "p1";
    player_help "last decision check" player1.last_decision None;
    player_help "folded check" player1.folded false;
    player_help "is AI check player 1" player1.is_AI false;
    player_help "is AI check player 2" player2.is_AI true;
    player_help "last decision check" player3.last_decision
      (Some "Call");
    player_help "player1 stack check" player3.stack 133;
    player_help "name check" player3.name "Clarkson";
    player_help "folded check" player3.folded false;
    player_help "last decision check" player4.last_decision
      (Some "Fold");
    player_help "player1 stack check" player4.stack 33;
    player_help "folded check" player4.folded true;
    player_help "is AI check player 1" player4.is_AI false;
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
  assert_equal expected_output (string_of_card card) ~printer:pp_string

let card_mod_tests =
  [
    init_card_test "First init test" Hearts 6
      { suit = Hearts; rank = 6 };
    init_card_test "Second init test" Spades 8
      { suit = Spades; rank = 8 };
    init_card_test "Third init test" Diamonds 9
      { suit = Diamonds; rank = 9 };
    get_suit_test "First get_suit test"
      { suit = Diamonds; rank = 3 }
      Diamonds;
    get_suit_test "Second get_suit test"
      { suit = Clubs; rank = 11 }
      Clubs;
    get_rank_test "First get_rank test" { suit = Hearts; rank = 6 } 6;
    get_rank_test "Second get_rank test" { suit = Spades; rank = 9 } 9;
    string_of_card_test "First string_of_card test"
      { suit = Clubs; rank = 14 }
      "\n......\n|♣   |\n| A  |\n|   ♣|\n......";
    string_of_card_test "Second string_of_card test"
      { suit = Diamonds; rank = 3 }
      "\n......\n|♦   |\n| 3  |\n|   ♦|\n......";
    string_of_card_test "Third string_of_card test"
      { suit = Hearts; rank = 7 }
      "\n......\n|♥   |\n| 7  |\n|   ♥|\n......";
    string_of_card_test "Fourth string_of_card test"
      { suit = Spades; rank = 9 }
      "\n......\n|♠   |\n| 9  |\n|   ♠|\n......";
    string_of_card_test "Fifth string_of_card test"
      { suit = Diamonds; rank = 2 }
      "\n......\n|♦   |\n| 2  |\n|   ♦|\n......";
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

let deal_hand_test name table =
  name >:: fun _ ->
  let c1, c2 = Table.deal_hand table in
  assert (
    Card.get_rank c1 <> Card.get_rank c2
    || Card.get_suit c1 <> Card.get_suit c2)

(** [raises_exn_test name f exn] is an OUnit test named [name] asserting
    that calling [f ()] raises exception [exn]. *)
let raise_exn_test name exn f = name >:: fun _ -> assert_raises exn f

(* Test decks *)
let start_deck = init_deck ()

let shuffled_deck = shuffle start_deck

let one_card_deck = [ { suit = Hearts; rank = 1 } ]

(* Test tables *)
let empty_table = create [] None

let start_table = init_table ()

let table_tests =
  [
    deck_size_test "[init_deck ()] has 52 elements" start_deck 52;
    full_deck_test "[init_deck ()] is a full deck" start_deck;
    deck_size_test "shuffling preserves deck size" shuffled_deck 52;
    full_deck_test "shuffling preserves elements of a deck"
      shuffled_deck;
    deal_hand_test "deal out two different cards for a hand" start_table;
  ]

(****************************************************************)
(* State *)
(****************************************************************)

let diff_hands_test name id1 id2 st =
  name >:: fun _ ->
  let card_1a, card_1b = (State.get_player id1 st).hand in
  let card_2a, card_2b = (State.get_player id2 st).hand in
  assert (
    card_1a <> card_2a && card_1a <> card_2b && card_1b <> card_2a
    && card_1b <> card_2b)

let chips_test name id st expected =
  name >:: fun _ ->
  assert_equal expected (State.chips id st) ~printer:string_of_int

let active_bet_test name st expected =
  name >:: fun _ ->
  assert_equal expected (State.active_bet st) ~printer:string_of_int

let active_players_test name st expected =
  name >:: fun _ -> assert_equal expected (State.active_players st)

let init_chips = 200

let new_match = State.init_state [ "dum1"; "dum2" ]

let bet1 = new_match |> State.bet "dum1" 0 50

let bet2 = bet1 |> State.bet "dum2" 0 100

let fold1 = bet2 |> State.fold "dum1"

let reset = fold1 |> State.reset [ "dum2" ]

let showdown = fold1 |> State.deal_center 5 |> State.showdown

let state_tests =
  [
    active_bet_test "init state starts w/ active bet of 0" new_match 0;
    active_players_test "all players in init state are active" new_match
      [ "dum1"; "dum2" ];
    chips_test "player 1 starts w/ 200 chips" "dum1" new_match 200;
    chips_test "player 2 starts w/ 200 chips" "dum2" new_match 200;
    diff_hands_test "all players in init state have different hands"
      "dum1" "dum2" new_match;
    active_bet_test "opening bet raises active bet" bet1 50;
    chips_test "betting subtracts chips from player" "dum1" bet1 150;
    active_bet_test "raising increases active bet" bet2 100;
    active_bet_test "folding doesn't affect active bet" fold1 100;
    active_players_test "folded players aren't active" fold1 [ "dum2" ];
    active_bet_test "resetting brings active bet back to 0" reset 0;
    active_players_test "all players in reset state are active" reset
      [ "dum1"; "dum2" ];
    chips_test "distribution of pot to winner via reset" "dum2" reset
      250;
    chips_test "none of the pot is given to loser during reset" "dum1"
      reset 150;
    active_bet_test "showdown resets active bet back to 0" showdown 0;
    active_players_test "all players after showdown are active" showdown
      [ "dum1"; "dum2" ];
  ]

(****************************************************************)
(* Command *)
(****************************************************************)
let parse_test name str expected =
  name >:: fun _ ->
  assert_equal expected (Command.parse str)
    ~printer:Command.string_of_cmd

let command_tests =
  [
    parse_test "parse bet cmd" "bet 50" (Bet 50);
    parse_test "parse raise cmd" "raise 50" (Raise 50);
    parse_test "parse check cmd" "Check " Check;
    parse_test "parse call cmd" "cALL" Call;
    parse_test "parse fold cmd" "    fold    " Fold;
    parse_test "invalid bet cmd" "Bet" Invalid;
    parse_test "invalid bet arg" "Bet x" Invalid;
    parse_test "invalid raise cmd" "Raise child" Invalid;
    parse_test "invalid spacing" "b et" Invalid;
    parse_test "wrong num of keywords" "bet 50 50" Invalid;
    parse_test "gibberish" "gibberish" Invalid;
  ]

(****************************************************************)
(* Hand Ranking *)
(****************************************************************)
let high_card1 =
  [
    init_card Spades 2;
    init_card Diamonds 3;
    init_card Hearts 4;
    init_card Spades 8;
    init_card Spades 11;
  ]

let high_card2 =
  [
    init_card Hearts 2;
    init_card Diamonds 4;
    init_card Diamonds 8;
    init_card Spades 12;
    init_card Clubs 13;
  ]

let pair1 =
  [
    init_card Hearts 10;
    init_card Spades 10;
    init_card Hearts 4;
    init_card Hearts 12;
    init_card Clubs 13;
  ]

let pair2 =
  [
    init_card Diamonds 14;
    init_card Hearts 14;
    init_card Diamonds 4;
    init_card Spades 8;
    init_card Clubs 9;
  ]

let two_pair1 =
  [
    init_card Diamonds 12;
    init_card Clubs 12;
    init_card Clubs 4;
    init_card Hearts 13;
    init_card Spades 4;
  ]

let two_pair2 =
  [
    init_card Spades 14;
    init_card Hearts 14;
    init_card Spades 7;
    init_card Hearts 7;
    init_card Diamonds 4;
  ]

let three_of_kind =
  [
    init_card Spades 14;
    init_card Hearts 14;
    init_card Clubs 14;
    init_card Spades 7;
    init_card Diamonds 4;
  ]

let straight1 =
  [
    init_card Clubs 3;
    init_card Hearts 4;
    init_card Clubs 5;
    init_card Hearts 6;
    init_card Diamonds 7;
  ]

let straight2 =
  [
    init_card Diamonds 4;
    init_card Spades 5;
    init_card Hearts 7;
    init_card Diamonds 6;
    init_card Clubs 3;
  ]

let flush1 =
  [
    init_card Clubs 4;
    init_card Clubs 5;
    init_card Clubs 7;
    init_card Clubs 14;
    init_card Clubs 3;
  ]

let flush2 =
  [
    init_card Clubs 2;
    init_card Clubs 6;
    init_card Clubs 8;
    init_card Clubs 12;
    init_card Clubs 13;
  ]

let full_house =
  [
    init_card Spades 4;
    init_card Clubs 4;
    init_card Hearts 4;
    init_card Spades 14;
    init_card Hearts 14;
  ]

let four_of_kind1 =
  [
    init_card Hearts 5;
    init_card Clubs 5;
    init_card Spades 5;
    init_card Diamonds 5;
    init_card Clubs 3;
  ]

let four_of_kind2 =
  [
    init_card Hearts 6;
    init_card Clubs 6;
    init_card Spades 6;
    init_card Diamonds 6;
    init_card Clubs 3;
  ]

let straight_flush =
  [
    init_card Clubs 4;
    init_card Clubs 5;
    init_card Clubs 7;
    init_card Clubs 6;
    init_card Clubs 3;
  ]

let royal =
  [
    init_card Clubs 10;
    init_card Clubs 11;
    init_card Clubs 12;
    init_card Clubs 14;
    init_card Clubs 13;
  ]

let compare_hands_test name hand1 hand2 expected =
  name >:: fun _ -> assert_equal expected (compare_hands hand1 hand2)

let hand_rank_tests =
  [
    compare_hands_test "identical hands" high_card1 high_card1 0;
    compare_hands_test "hand vs. higher hand" high_card1 high_card2 (-1);
    compare_hands_test "pair vs. high card" pair1 high_card2 1;
    compare_hands_test "higher pair vs. pair" pair2 pair1 1;
    compare_hands_test "pair vs. 2pair" pair2 two_pair1 (-1);
    compare_hands_test "higher 2pair vs. lower" two_pair2 two_pair1 1;
    compare_hands_test "3ofkind vs. 2pair" three_of_kind two_pair1 1;
    compare_hands_test "3ofkind vs. straight" three_of_kind straight1
      (-1);
    compare_hands_test "straight tie" straight1 straight2 0;
    compare_hands_test "straight vs. pair" straight1 pair1 1;
    compare_hands_test "flush vs. straight" flush1 straight1 1;
    compare_hands_test "higher flush vs. lower" flush1 flush2 1;
    compare_hands_test "full house vs. flush" full_house flush1 1;
    compare_hands_test "full house vs. four of kind" full_house
      four_of_kind1 (-1);
    compare_hands_test "four of kind higher vs. lower" four_of_kind1
      four_of_kind2 (-1);
    compare_hands_test "straight flush vs. four of kind" straight_flush
      four_of_kind2 1;
    compare_hands_test "royal vs. straight flush" royal straight_flush 1;
  ]

let suite =
  "test suite for MS1"
  >::: List.flatten
         [
           player_tests;
           card_mod_tests;
           table_tests;
           state_tests;
           command_tests;
           hand_rank_tests;
         ]

let _ = run_test_tt_main suite
