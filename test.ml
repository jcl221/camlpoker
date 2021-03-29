open OUnit2
open Player
open Table
open Card


let new_deck = init_deck ()
let shuffled_deck = shuffle new_deck

let hand1 = [{suit = Spades; rank = 10}; {suit = Hearts; rank = 12}]
let hand2 = [{suit = Clubs; rank = 14}; {suit = Diamonds; rank = 8}]
let string_of_hand1 = "10 of Spades and Queen of Hearts"
let string_of_hand2 = "Ace of Clubs and 8 of Diamonds"
let player1 = player_init "p1" hand1
let player2 = player_init "AI" hand2
let player_tests = 
  [
    "p1 hand to string" >:: (fun _ -> assert_equal string_of_hand1
      (hand_to_string player1));
    "p2 hand to string" >:: (fun _ -> assert_equal string_of_hand2
      (hand_to_string player2));
    "stack check" >:: (fun _ -> assert_equal 200 player1.stack);
    "name check" >:: (fun _ -> assert_equal "p1" player1.name);
    "last decision check" >:: (fun _ -> assert_equal None player1.last_decision);
    "folded check" >:: (fun _ -> assert_equal false player1.folded);
    "is AI check player 1" >:: (fun _ -> assert_equal false player1.is_AI);
    "is AI check player 2" >:: (fun _ -> assert_equal true player1.is_AI);
  ]
