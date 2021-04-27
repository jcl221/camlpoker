(** AF: {players; action_queue; table; pot; winner } 
    is the game state with 
      * players listed in [players],
      * players that are pending an action listed in [action_queue],
      * a table (with deck and community cards) [table],
      * a pot of amount [pot], 
      * a winner [winner].

    RI: * [players] contains < 6 people. 
        * [action_queue] is empty or has guest that exists in [players]
        * [pot] is nonnegative.
        * [winner] is an existing guest in [players]. *)
type t = {
  players : Player.player list;
  active_bet : int;
  table : Table.table;
  pot : int;
}

exception Empty_Hand

let init_state ids =
  let starting_table = Table.init_table () in
  {
    players = List.map (Player.player_init starting_table) ids;
    active_bet = 0;
    table = starting_table;
    pot = 0;
  }

let deal_center st = { st with table = Table.place_center st.table }

let fold st id =
  let survey (p : Player.player) =
    if p.name = id then { p with folded = true } else p
  in
  let updated_players = List.map survey st.players in
  { st with players = updated_players }

let bet st id amt =
  let survey (p : Player.player) =
    if p.name = id then { p with stack = p.stack - amt } else p
  in
  let updated_players = List.map survey st.players in
  { st with players = updated_players; pot = st.pot + amt }

(** [compare_cards c1 c2] is 1 if c1's rank is greater than c2's and -1
    otherwise. If they are equivalent, it is 0.*)
let compare_cards (c1 : Card.t) (c2 : Card.t) =
  if c1.rank > c2.rank then 1
  else if c1.rank < c2.rank then -1
  else 0

(** [ranks_lst hand] is a sorted list of the ranks of the cards
    in [hand].*)
let ranks_lst hand =
  let sorted = List.sort compare_cards hand in
  List.map (fun (x : Card.t) -> x.rank) sorted

(** [suits_lst hand] is a list of all the suits in [hand].*)
let suits_lst hand = List.map (fun (x : Card.t) -> x.suit) hand

(** [highest_rank hand] is the highest rank of the cards in [hand]*)
let highest_rank hand =
  let ranks = ranks_lst hand in
  match ranks with
  | _ :: _ :: _ :: _ :: r5 :: [] -> r5
  | _ -> raise (Invalid_argument "hand is not a valid input")

(** [high_card hand] is a tuple option with 1 and the list of ranks sorted in
    descending order*)
let high_card hand =
  let desc_ranks = List.rev (ranks_lst hand) in
  Some (1, desc_ranks)

(** [four_of_a_kind hand] is a tuple option with 8 and the list of ranks with
    the four similar cards at the front and the different one at the
    end. If there is no four of a kind, it is None*)
let four_of_a_kind hand =
  let ranks = ranks_lst hand in
  match ranks with
  | r1 :: r2 :: r3 :: r4 :: r5 :: [] -> begin
    if ((r1 = r2) && (r2 = r3) && (r3 = r4))
      then Some (8, r1 :: r2 :: r3 :: r4 :: r5 :: [])
    else if ((r2=r3) && (r3 = r4) && (r4 = r5))
      then Some (8, r2 :: r3 :: r4 :: r5 :: r1 :: [])
    else None
  end
  | _ -> raise (Invalid_argument "hand is invalid")

(** [three_of_a_kind hand] is a tuple option with 4 and the list of ranks with
    the three similar cards at the front and the two different ones at the
    end in descending order. If there is no three of a kind, it is None*)
let three_of_a_kind hand =
  let ranks = ranks_lst hand in
  match ranks with
  | r1 :: r2 :: r3 :: r4 :: r5 :: [] -> begin
    if ((r1 = r2) && (r2 = r3))
      then Some (4, r1 :: r2 :: r3 :: r4 :: r5 :: [])
    else if ((r2 = r3) && (r3 = r4))
      then Some (4, r2 :: r3 :: r4 :: r5 :: r1 :: [])
    else if ((r3 = r4) && (r4 = r5))
      then Some (4, r3 :: r4 :: r5 :: r2 :: r1 :: [])
    else None
  end
  | _ -> raise (Invalid_argument "hand is invalid")

(** [straight hand] is a tuple option with 5 and the list of ranks in
    descending order if the hand is a straight. If it is not, then it is None*)
let straight hand =
  let ranks = ranks_lst hand in
  match ranks with
  | r1 :: r2 :: r3 :: r4 :: r5 :: [] -> begin
    if ((r1 = r2 - 1) && (r2 = r3 - 1 - 1) && (r4 = r5 - 1))
      then Some (5, [r5; r4; r3; r2; r1])
    else if ((r1 = 2) && (r2 = 3) && (r3 = 4) && (r4 = 5) && (r5 = 14))
      then Some (5, [r4; r3; r2; r1; r5])
    else None
  end
  | _ -> raise (Invalid_argument "hand is invalid")

(** [flush hand] is a tuple option with 6 and the list of ranks in
    descending order if the hand is a flush. If it is not, then it is None*)
let flush hand =
  let suits = suits_lst hand in
  match suits with
  | s1 :: s2 :: s3 :: s4 :: s5 :: [] -> begin
    if ((s1 = s2) && (s2 = s3) && (s3 = s4) && (s4 = s5))
    then Some (6, List.rev (ranks_lst hand))
    else None
  end
  | _ -> raise (Invalid_argument "hand is invalid")

(** [straight_flush hand] is a tuple option with 9 and the list of ranks
    in descending order if the hand is a straight flush. If it is not, then it
    is None*)
let straight_flush hand =
  if ((straight hand != None) && (flush hand != None))
  then Some (9, List.rev (ranks_lst hand))
  else None

(** [royal_flush hand] is a tuple option with 10 and the list of cards
    in descending order. If no royal flush, then it is None*)
let royal_flush hand =
  if ((straight hand != None) && (flush hand != None)
    && (highest_rank hand = 14))
  then Some (10, List.rev (ranks_lst hand))
  else None

(** [full_house hand] is a tuple option with 7 and the list of ranks with
    the three of a kind in the front and the pair in the back. If no full
    house, then it is None*)
let full_house hand =
  let ranks = ranks_lst hand in
  match ranks with
  | r1 :: r2 :: r3 :: r4 :: r5 :: [] -> begin
    if ((r1 = r2) && (r2 = r3) && (r4= r5)) then
      Some (7, [r1; r2; r3; r4; r5])
    else if ((r3 = r4) && (r4 = r5) && (r1= r2)) then
      Some (7, [r3; r4; r5; r1; r2])
    else None
  end
  | _ -> raise (Invalid_argument "hand is invalid")

(** [num_pairs ranks acc] is the number of pairs in a rank list*)
let rec num_pairs ranks acc =
  match ranks with
  | [] -> acc
  | h :: t -> if List.mem h t then num_pairs t (acc + 1) else num_pairs t acc

(** [highest_pair ranks acc] is the highest pair in a rank list.
    Requires: ranks is sorted using the ranks_lst function.*)
let rec highest_pair ranks acc =
  match ranks with
  | [] -> acc
  | h :: t -> if List.mem h t then highest_pair t h else highest_pair t acc

(** [pair_helper rank ranks] is a list with the same elements as ranks
    with the pair of rank [rank] at the leftmost part of the list and the
    remaining elements following in descending order*)
let pair_helper rank ranks =
  let first_two = [rank; rank] in
  let not_paired_ranks = 
    List.filter (fun x -> (List.mem x first_two) = false) ranks in
  first_two @ List.rev (List.sort compare not_paired_ranks)

(** [pair hand] is a tuple option with 2 and the five ranks such that
    the paired ranks are leftmost and the other cards are following in
    descending order. If there is no pair, it is None. *)
let pair hand =
  let ranks = ranks_lst hand in
  if ((three_of_a_kind hand != None) || (four_of_a_kind hand != None)
    || full_house hand != None) then None
  else
    let rank = highest_pair ranks 0 in
    let pair_list = pair_helper rank ranks in
    if num_pairs ranks 0 = 1 then Some (2, pair_list) else None

(** [same_rank_list ranks acc] is a list of the pairs in a rank list*)
let rec same_rank_list ranks acc =
  match ranks with
  | [] -> acc
  | h :: t -> if List.mem h t then same_rank_list t (h::h::acc)
    else same_rank_list t acc

(** [two_pair_help values ranks] is the list of ranks in ranks with the
    pairs at the leftmost of the list in descending order and the different
    caard at the end*)
let two_pair_help values ranks =
  let first_four = List.rev (List.sort compare values) in
  first_four @ List.filter (fun x -> (List.mem x first_four) = false) ranks

(** [two_pair hand] is a tuple option with 3 and a list of ranks so that
    the pairs are on the left in descending order. If there is no two pair,
    then it is None. *)
let two_pair hand =
  let ranks = ranks_lst hand in
  if ((three_of_a_kind hand != None) || (four_of_a_kind hand != None)
    || full_house hand != None) then None
  else
    let num_of_pairs = num_pairs ranks 0 in
    let values = same_rank_list ranks [] in
    let list_of_hand = two_pair_help values ranks in
    if num_of_pairs = 2 then Some (3, list_of_hand) else None
let showdown st = { st with pot = 0 }

let get_player st id =
  let rec get_player_from_lst (lst : Player.player list) id =
    match lst with
    | [] -> raise (Failure "Not Found")
    | h :: t -> if h.name = id then h else get_player_from_lst t id
  in
  get_player_from_lst st.players id

let player_hand (player : Player.player) = (player.name, player.hand)

let player_hands st = List.map player_hand st.players
