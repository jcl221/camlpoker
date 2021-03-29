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

let suite =
  "test suite for MS1"
    >::: List.flatten [ player_tests ]
  
let _ = run_test_tt_main suite
