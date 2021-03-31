exception Invalid_Deck

exception Empty_Deck

open Card

type deck = Card.t list

(** AF: [table] is a tuple representing all cards in the deck and the
    commmunity cards on the board. RI: There can be no more than 52
    cards in the deck and no duplicates*)
type table = deck * Card.t list option

(** [int_to_suit n] is the suit to which [n] maps.  *)
let int_to_suit n =
  match n with 0 -> Hearts | 1 -> Diamonds | 2 -> Spades | _ -> Clubs

let init_deck () =
  let lst = ref [] in
  for x = 0 to 3 do
    for y = 2 to 14 do
      let card = init_card (int_to_suit x) y in
      lst := card :: !lst
    done
  done;
  !lst

(** [contains_card card deck] is whether the deck [deck] contains card
    [card]. *)
let contains_card ?print_missing:(pm = false) card deck =
  match List.find_opt (fun c -> c = card) deck with
  | Some x -> true
  | None ->
      if pm then print_endline ("\nmissing" ^ string_of_card card);
      false

(** Efficiency: O(n^2). *)
let assert_valid_start deck =
  assert (List.length deck = 52);
  for s = 0 to 3 do
    let suit = int_to_suit s in
    for r = 2 to 14 do
      let card = Card.init_card suit r in
      assert (contains_card card deck ~print_missing:true)
    done
  done

(** [shuffle_help shuff current] is the deck of cards obtained by randomly 
    shuffling the deck [current]. 
    Requires: [current] is a valid starting deck. 
    Efficiency: O(n^2). *)
let rec shuffle_help (shuffled : deck) current =
  if List.length shuffled = 52 then shuffled
  else begin
    Random.self_init ();
    let index = Random.int (List.length current) in
    let select = List.nth current index in
    let updated_current = List.filter (fun x -> x <> select) current in
    shuffle_help (select :: shuffled) updated_current
  end

let shuffle (c : deck) =
  assert_valid_start c;
  shuffle_help [] c

let init_table () =
  let start_deck = () |> init_deck |> shuffle in
  assert_valid_start start_deck;
  (start_deck, None)

let create cards board : table = (cards, board)

let place_center table =
  let deck = fst table in
  let board = snd table in
  match deck with
  | [] -> raise Empty_Deck
  | x :: xs -> (
      match board with
      | None -> create xs (Some [ x ])
      | Some cards -> create xs (Some (x :: cards)))

let deal_one_hand (d : deck) =
  match d with
  | card1 :: card2 :: _ -> (card1, card2)
  | _ -> raise Empty_Deck
