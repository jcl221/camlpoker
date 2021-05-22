open Player

(** AF: \{ players; table; active_bet; pot \} is the game state with *
    players listed in [players], * an active bet amount of [active_bet],
    * a table (with deck and community cards) [table], * a pot of amount
    [pot],

    RI: [players] contains < 6 people. [active_bet] and [pot] are
    nonnegative. *)
type t = {
  players : player list;
  table : Table.table;
  active_bet : int;
  pot : int;
  ai_difficulty : string;
}

type stage =
  | Preflop
  | Midgame
  | Showdown

exception Empty_Hand

exception Tie

let init_state ids =
  let starting_table = Table.init_table () in
  {
    players = List.map (player_init 200 starting_table) ids;
    active_bet = 0;
    table = starting_table;
    pot = 0;
    ai_difficulty = "easy";
  }

let stage_of_game st =
  match st.table.board with
  | None -> Preflop
  | Some lst -> if List.length lst = 5 then Showdown else Midgame

let active_players st =
  st.players
  |> List.filter (fun (p : player) -> not p.folded)
  |> List.map (fun (p : player) -> p.name)

let active_bet st = st.active_bet

let get_player id st =
  let rec get_player_from_lst (lst : player list) id =
    match lst with
    | h :: t -> if h.name = id then h else get_player_from_lst t id
    | [] -> raise (Failure "Not Found")
  in
  get_player_from_lst st.players id

let chips name st = (get_player name st).stack

let rec deal_center count st =
  let st' =
    let table = Table.place_center st.table in
    { st with table; active_bet = 0 }
  in
  if count <= 1 then st' else deal_center (count - 1) st'

let fold id st =
  let survey (p : player) =
    if p.name = id then { p with folded = true } else p
  in
  let updated_players = List.map survey st.players in
  { st with players = updated_players }

let bet id current raise_to st =
  assert (raise_to > current && raise_to >= st.active_bet);

  let amt = raise_to - current in
  let scan (p : player) =
    if p.name = id then { p with stack = p.stack - amt } else p
  in
  let updated_players = List.map scan st.players in
  {
    st with
    players = updated_players;
    active_bet = raise_to;
    pot = st.pot + amt;
  }

let reset winners st =
  List.iter (fun name -> print_endline (name ^ " wins!")) winners;
  let new_table = Table.init_table () in
  let winnings = st.pot / List.length winners in
  let reset (p : Player.player) =
    let chips =
      if List.mem p.name winners then p.stack + winnings else p.stack
    in
    Player.player_init chips new_table p.name
  in
  let next_lobby = List.map reset st.players in
  {
    players = next_lobby;
    table = new_table;
    active_bet = 0;
    pot = 0;
    ai_difficulty = st.ai_difficulty;
  }

(** [print_players main_user mask lst] prints information for all
    players in [lst] out onto stdout. If [mask] is true, obscures the
    hands of all players except for the player with name [main_user]. *)
let rec print_players main_user mask = function
  | [] -> print_string "\n"
  | h :: t ->
      let profile = Player.player_info h in
      let cards =
        if h.folded then "Folded"
        else if mask && h.name <> main_user then "Hidden"
        else Player.string_of_hand h
      in
      print_endline (profile ^ ": " ^ cards);
      print_players main_user mask t

let print_state st main_user mask =
  print_endline "\n====== Table ======";
  print_endline ("Community Cards: " ^ Table.string_of_table st.table);
  print_endline "\n====== Players =====";
  print_players main_user mask st.players

(***************************************************************************)
(* Hand Ranking *)
(***************************************************************************)

(** [compare_cards c1 c2] is 1 if c1's rank is greater than c2's and -1
    otherwise. If they are equivalent, it is 0.*)
let compare_cards (c1 : Card.t) (c2 : Card.t) =
  if c1.rank > c2.rank then 1 else if c1.rank < c2.rank then -1 else 0

(** [ranks_lst hand] is a sorted list of the ranks of the cards in
    [hand].*)
let ranks_lst hand =
  let sorted = List.sort compare_cards hand in
  List.map (fun (x : Card.t) -> x.rank) sorted

(** [suits_lst hand] is a list of all the suits in [hand].*)
let suits_lst hand = List.map (fun (x : Card.t) -> x.suit) hand

(** [highest_rank hand] is the highest rank of the cards in [hand]*)
let highest_rank hand =
  let ranks = ranks_lst hand in
  match ranks with
  | [ _; _; _; _; r5 ] -> r5
  | _ -> raise (Invalid_argument "hand is not a valid input")

(** [high_card hand] is a tuple option with 1 and the list of ranks
    sorted in descending order*)
let high_card hand =
  let desc_ranks = List.rev (ranks_lst hand) in
  Some (1, desc_ranks)

(** [four_of_a_kind hand] is a tuple option with 8 and the list of ranks
    with the four similar cards at the front and the different one at
    the end. If there is no four of a kind, it is None*)
let four_of_a_kind hand =
  let ranks = ranks_lst hand in
  match ranks with
  | [ r1; r2; r3; r4; r5 ] ->
      if r1 = r2 && r2 = r3 && r3 = r4 then
        Some (8, [ r1; r2; r3; r4; r5 ])
      else if r2 = r3 && r3 = r4 && r4 = r5 then
        Some (8, [ r2; r3; r4; r5; r1 ])
      else None
  | _ -> raise (Invalid_argument "hand is invalid")

(** [three_of_a_kind hand] is a tuple option with 4 and the list of
    ranks with the three similar cards at the front and the two
    different ones at the end in descending order. If there is no three
    of a kind, it is None*)
let three_of_a_kind hand =
  let ranks = ranks_lst hand in
  match ranks with
  | [ r1; r2; r3; r4; r5 ] ->
      if r1 = r2 && r2 = r3 then Some (4, [ r1; r2; r3; r4; r5 ])
      else if r2 = r3 && r3 = r4 then Some (4, [ r2; r3; r4; r5; r1 ])
      else if r3 = r4 && r4 = r5 then Some (4, [ r3; r4; r5; r2; r1 ])
      else None
  | _ -> raise (Invalid_argument "hand is invalid")

(** [straight hand] is a tuple option with 5 and the list of ranks in
    descending order if the hand is a straight. If it is not, then it is
    None*)
let straight hand =
  let ranks = ranks_lst hand in
  match ranks with
  | [ r1; r2; r3; r4; r5 ] ->
      if r1 = r2 - 1 && r2 = r3 - 1 && r3 = r4 - 1 && r4 = r5 - 1 then
        Some (5, [ r5; r4; r3; r2; r1 ])
      else if r1 = 2 && r2 = 3 && r3 = 4 && r4 = 5 && r5 = 14 then
        Some (5, [ r4; r3; r2; r1; r5 ])
      else None
  | _ -> raise (Invalid_argument "hand is invalid")

(** [flush hand] is a tuple option with 6 and the list of ranks in
    descending order if the hand is a flush. If it is not, then it is
    None*)
let flush hand =
  let suits = suits_lst hand in
  match suits with
  | [ s1; s2; s3; s4; s5 ] ->
      if s1 = s2 && s2 = s3 && s3 = s4 && s4 = s5 then
        Some (6, List.rev (ranks_lst hand))
      else None
  | _ -> raise (Invalid_argument "hand is invalid")

(** [straight_flush hand] is a tuple option with 9 and the list of ranks
    in descending order if the hand is a straight flush. If it is not,
    then it is None*)
let straight_flush hand =
  if straight hand != None && flush hand != None then
    Some (9, List.rev (ranks_lst hand))
  else None

(** [royal_flush hand] is a tuple option with 10 and the list of cards
    in descending order. If no royal flush, then it is None*)
let royal_flush hand =
  if
    straight hand != None
    && flush hand != None
    && highest_rank hand = 14
  then Some (10, List.rev (ranks_lst hand))
  else None

(** [full_house hand] is a tuple option with 7 and the list of ranks
    with the three of a kind in the front and the pair in the back. If
    no full house, then it is None*)
let full_house hand =
  let ranks = ranks_lst hand in
  match ranks with
  | [ r1; r2; r3; r4; r5 ] ->
      if r1 = r2 && r2 = r3 && r4 = r5 then
        Some (7, [ r1; r2; r3; r4; r5 ])
      else if r3 = r4 && r4 = r5 && r1 = r2 then
        Some (7, [ r3; r4; r5; r1; r2 ])
      else None
  | _ -> raise (Invalid_argument "hand is invalid")

(** [num_pairs ranks acc] is the number of pairs in a rank list*)
let rec num_pairs ranks acc =
  match ranks with
  | [] -> acc
  | h :: t ->
      if List.mem h t then num_pairs t (acc + 1) else num_pairs t acc

(** [highest_pair ranks acc] is the highest pair in a rank list.
    Requires: ranks is sorted using the ranks_lst function.*)
let rec highest_pair ranks acc =
  match ranks with
  | [] -> acc
  | h :: t ->
      if List.mem h t then highest_pair t h else highest_pair t acc

(** [pair_helper rank ranks] is a list with the same elements as ranks
    with the pair of rank [rank] at the leftmost part of the list and
    the remaining elements following in descending order*)
let pair_helper rank ranks =
  let first_two = [ rank; rank ] in
  let not_paired_ranks =
    List.filter (fun x -> List.mem x first_two = false) ranks
  in
  first_two @ List.rev (List.sort compare not_paired_ranks)

(** [pair hand] is a tuple option with 2 and the five ranks such that
    the paired ranks are leftmost and the other cards are following in
    descending order. If there is no pair, it is None. *)
let pair hand =
  let ranks = ranks_lst hand in
  if
    three_of_a_kind hand != None
    || four_of_a_kind hand != None
    || full_house hand != None
  then None
  else
    let rank = highest_pair ranks 0 in
    let pair_list = pair_helper rank ranks in
    if num_pairs ranks 0 = 1 then Some (2, pair_list) else None

(** [same_rank_list ranks acc] is a list of the pairs in a rank list*)
let rec same_rank_list ranks acc =
  match ranks with
  | [] -> acc
  | h :: t ->
      if List.mem h t then same_rank_list t (h :: h :: acc)
      else same_rank_list t acc

(** [two_pair_help values ranks] is the list of ranks in ranks with the
    pairs at the leftmost of the list in descending order and the
    different caard at the end*)
let two_pair_help values ranks =
  let first_four = List.rev (List.sort compare values) in
  first_four
  @ List.filter (fun x -> List.mem x first_four = false) ranks

(** [two_pair hand] is a tuple option with 3 and a list of ranks so that
    the pairs are on the left in descending order. If there is no two
    pair, then it is None. *)
let two_pair hand =
  let ranks = ranks_lst hand in
  if
    three_of_a_kind hand != None
    || four_of_a_kind hand != None
    || full_house hand != None
  then None
  else
    let num_of_pairs = num_pairs ranks 0 in
    let values = same_rank_list ranks [] in
    let list_of_hand = two_pair_help values ranks in
    if num_of_pairs = 2 then Some (3, list_of_hand) else None

(** [combnk k lst] is all combinations of length k in list [lst]. Credit
    for the algorithm and implementation is given to
    https://codereview.stackexchange.com/questions/40366/combinations-of-size-k-from-a-list-in-ocaml*)
let rec combnk k lst =
  if k = 0 then [ [] ]
  else
    let rec combnk_help = function
      | [] -> []
      | h :: t ->
          List.map (fun x -> h :: x) (combnk (k - 1) t) :: combnk_help t
    in
    List.concat (combnk_help lst)

(** [every_hand pl st] is all the possible 5 hand combinations for
    player [pl] in state [st]. *)
let every_hand (pl : Player.player) st =
  let pl_two_cards = [ fst pl.hand; snd pl.hand ] in
  let table = st.table in
  match table.board with
  | Some [ c1; c2; c3; c4; c5 ] ->
      combnk 5 ([ c1; c2; c3; c4; c5 ] @ pl_two_cards)
  | _ -> raise (Invalid_argument "table is not filled completely")

(** [best_hand hand] is a tuple option with the value of best hand from
    a given card list [hand] and the list of that hand in descending
    order. *)
let best_hand hand =
  if royal_flush hand != None then royal_flush hand
  else if straight_flush hand != None then straight_flush hand
  else if four_of_a_kind hand != None then four_of_a_kind hand
  else if full_house hand != None then full_house hand
  else if flush hand != None then flush hand
  else if straight hand != None then straight hand
  else if three_of_a_kind hand != None then three_of_a_kind hand
  else if two_pair hand != None then two_pair hand
  else if pair hand != None then pair hand
  else high_card hand

(** [better_hand hand1 hand2] is true if hand1 ranks higher than hand2
    according to Texas Holdem rules. *)
let better_hand hand1 hand2 =
  (* [out_of_option opt] converts opt of the form Some x to x. *)
  let out_of_option opt =
    match opt with
    | Some x -> x
    | None -> raise (Invalid_argument "opt is not filled")
  in

  let h1 = out_of_option hand1 in
  let h2 = out_of_option hand2 in

  let first1 = List.nth (snd h1) 0 in
  let first2 = List.nth (snd h2) 0 in
  let second1 = List.nth (snd h1) 1 in
  let second2 = List.nth (snd h2) 1 in
  let third1 = List.nth (snd h1) 2 in
  let third2 = List.nth (snd h2) 2 in
  let fourth1 = List.nth (snd h1) 3 in
  let fourth2 = List.nth (snd h2) 3 in
  let fifth1 = List.nth (snd h1) 4 in
  let fifth2 = List.nth (snd h2) 4 in

  if fst h1 = fst h2 then
    if first1 = first2 then
      if second1 = second2 then
        if third1 = third2 then
          if fourth1 = fourth2 then
            if fifth1 = fifth2 then raise Tie else fifth1 > fifth2
          else fourth1 > fourth2
        else third1 > third2
      else second1 > second2
    else first1 > first2
  else fst h1 > fst h2

(** [compare_hands h1 h2] is [1] if [h1] is a better hand than [h2],
    [-1] if it is worse, and 0 if the hands are tied. *)
let compare_hands h1 h2 =
  try
    if better_hand (best_hand h1) (best_hand h2) then 1
    else if better_hand (best_hand h2) (best_hand h1) then -1
    else 0
  with Tie -> 0

(** [best_player_hand pl st] is the best hand player [pl] has given her
    hand and the board of the table in state [st]. *)
let best_player_hand pl st =
  let all_hands = every_hand pl st in
  let all_sorted_hands = List.rev (List.sort compare_hands all_hands) in
  List.nth all_sorted_hands 0

(** [player_with_best_hand st] is a list of players with the best hand
    of all the players in state [st]. *)
let player_with_best_hand st =
  let players = st.players in

  let rec find_best_player (pls : Player.player list) acc =
    if acc = [] then
      match pls with
      | [] -> raise (Failure "impossible")
      | h :: t -> find_best_player t (h :: acc)
    else
      match pls with
      | [] -> acc
      | p1 :: t -> (
          let acc_cards = best_player_hand (List.nth acc 0) st in
          let p1_cards = best_player_hand p1 st in
          match compare_hands acc_cards p1_cards with
          | 1 -> find_best_player t acc
          | -1 -> find_best_player t [ p1 ]
          | _ -> find_best_player t (p1 :: acc))
  in
  find_best_player players []

let showdown st =
  let winning_ids =
    List.map
      (fun (x : Player.player) -> x.name)
      (player_with_best_hand st)
  in
  reset winning_ids st

let player_hands st =
  let player_hand (p : Player.player) = (p.name, p.hand) in
  List.map player_hand st.players
