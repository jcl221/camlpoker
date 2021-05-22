open Card
open Player
open Table
open State
open Command

(***************************************************************************)
(* Hand Ranking from state.ml*)
(***************************************************************************)

(** [compare_cards c1 c2] is 1 if c1's rank is greater than c2's and -1
    otherwise. If they are equivalent, it is 0.*)
let compare_cards (c1 : Card.t) (c2 : Card.t) =
  if c1.rank > c2.rank then 1 else if c1.rank < c2.rank then -1 else 0

(** [ranks_lst hand] is a sorted list of the ranks of the cards
    in [hand].*)
let ranks_lst hand =
  let sorted = List.sort compare_cards hand in
  List.map (fun (x : Card.t) -> x.rank) sorted

(** [suits_lst hand] is a list of all the suits in [hand].*)
let suits_lst hand = 
  let get_suit_value suit =
    match suit with
    | Spades -> 1
    | Clubs -> 2
    | Hearts -> 3
    | Diamonds -> 4
  in
  let suit_compare card1 card2 =
    let s1 = card1.suit in
    let s2 = card2.suit in
    let s1_value = get_suit_value s1 in
    let s2_value = get_suit_value s2 in
    compare s1_value s2_value
  in
  List.sort suit_compare hand

(** [highest_rank hand] is the highest rank of the cards in [hand]*)
let highest_rank hand =
  let ranks = List.rev (ranks_lst hand) in
  match ranks with
  | high :: t -> high
  | _ -> raise (Invalid_argument "invalid hand")


(** [high_card hand] is a tuple option with 1 and the list of ranks sorted in
    descending order*)
let high_card hand =
  let rank = highest_rank hand in
  if rank > 10 then Some (1, hand) else Some (0, hand)

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
    with the pair of rank [rank] at the leftmost part of the list and the
    remaining elements following in descending order*)
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
  let sorted = ranks_lst hand in
  let rec pair_help sorted =
    match sorted with
    | r1 :: r2 :: t -> begin
      if r1 = r2 then Some (2, hand) else pair_help (r2 :: t)
    end
    | _ -> None
  in
  pair_help sorted

(** [same_rank_list ranks acc] is a list of the pairs in a rank list*)
let rec same_rank_list ranks acc =
  match ranks with
  | [] -> acc
  | h :: t ->
      if List.mem h t then same_rank_list t (h :: h :: acc)
      else same_rank_list t acc

(** [two_pair_help values ranks] is the list of ranks in ranks with the
    pairs at the leftmost of the list in descending order and the different
    caard at the end*)
let two_pair_helper values ranks =
  let first_four = List.rev (List.sort compare values) in
  first_four
  @ List.filter (fun x -> List.mem x first_four = false) ranks

(** [two_pair hand] is a tuple option with 3 and a list of ranks so that
    the pairs are on the left in descending order. If there is no two pair,
    then it is None. *)
let two_pair hand =
  let sorted = ranks_lst hand in
  let rec two_pair_help sorted already_found =
    match sorted with
    | r1 :: r2 :: t -> begin
      if r1 = r2 && already_found
        then Some (3, hand)
      else if r1 = r2
        then two_pair_help t true
      else
        two_pair_help (r2 :: t) already_found
    end
    | _ -> None
  in
  two_pair_help sorted false

(** [three_of_a_kind hand] is a tuple option with 4 and the list of ranks with
    the three similar cards at the front and the two different ones at the
    end in descending order. If there is no three of a kind, it is None*)
let three_of_a_kind hand =
  if List.length hand < 3 then pair hand
  else
    let sorted = ranks_lst hand in
    let rec three_of_kind_help sorted =
      match sorted with
      | r1 :: r2 :: r3 :: t -> begin
        if r1 = r2 && r2 = r3
          then Some (4, hand)
        else
          three_of_kind_help (r2 :: r3 :: t)
      end
      | _ -> None
    in
    three_of_kind_help sorted

(** [straight hand] is a tuple option with 5 and the list of ranks in
    descending order if the hand is a straight. If it is not, then it is None*)
let straight hand =
  let sorted = ranks_lst hand in
  let rec straight_help sorted =
    if List.mem 2 sorted && List.mem 3 sorted && List.mem 4 sorted
      && List.mem 5 sorted && List.mem 14 sorted 
      then Some (5, hand)
    else
      match sorted with
      | r1 :: r2 :: t -> begin
        if r1 + 1 = r2 then straight_help (r2 :: t) else None
      end
      | _ -> Some (5, hand)
  in
  match sorted with
  | [r1; r2; r3; r4; r5; r6] -> begin
    if straight_help [r1; r2; r3; r4; r5] = None
      then straight_help [r2; r3; r4; r5; r6]
    else straight_help [r1; r2; r3; r4; r5]
  end
  | [r1; r2; r3; r4; r5; r6; r7] -> begin
    if straight_help [r1; r2; r3; r4; r5] = None
      then
        if straight_help [r2; r3; r4; r5; r6] = None
          then straight_help [r3; r4; r5; r6; r7]
        else straight_help [r2; r3; r4; r5; r6]
    else straight_help [r1; r2; r3; r4; r5]
  end
  | _ -> straight_help sorted

(** [flush hand] is a tuple option with 6 and the list of ranks in
    descending order if the hand is a flush. If it is not, then it is None*)
let flush hand =
  let suits = suits_lst hand in
  let rec flush_help suits =
    match suits with
    | suit1 :: suit2 :: t -> begin
      if suit1 = suit2 then flush_help (suit2 :: t) else None
    end
    | _ -> Some (6, hand)
  in
  match suits with
  | [s1; s2; s3; s4; s5; s6] -> begin
    if flush_help [s1; s2; s3; s4; s5] = None
      then flush_help [s2; s3; s4; s5; s6]
    else flush_help [s1; s2; s3; s4; s5]
  end
  | [s1; s2; s3; s4; s5; s6; s7] -> begin
    if flush_help [s1; s2; s3; s4; s5] = None
      then
        if flush_help [s2; s3; s4; s5; s6] = None
          then flush_help [s3; s4; s5; s6; s7]
        else flush_help [s2; s3; s4; s5; s6]
    else flush_help [s1; s2; s3; s4; s5]
  end
  | _ -> flush_help suits

  (** [full_house hand] is a tuple option with 7 and the list of ranks with
    the three of a kind in the front and the pair in the back. If no full
    house, then it is None*)
let full_house hand =
  let sorted = ranks_lst hand in
  let rec full_house_help sorted already_pair already_three =
    match sorted with
    | r1 :: r2 :: r3 :: t -> begin
      if r1 = r2 && r2 = r3 && already_pair then Some (7, hand)
      else if r1 = r2 && r2 = r3 then full_house_help t already_pair true
      else if r1 = r2 && already_three then Some (7, hand)
      else if r1 = r2 then full_house_help (r3 :: t) true already_three
      else full_house_help (r2 :: r3 :: t) already_pair already_three
    end
    | [r1; r2] -> begin
      if r1 = r2 && already_three then Some (7, hand) else None
    end
    | _ -> None
  in
  full_house_help sorted false false

(** [four_of_a_kind hand] is a tuple option with 8 and the list of ranks with
    the four similar cards at the front and the different one at the
    end. If there is no four of a kind, it is None*)
let four_of_a_kind hand =
  if List.length hand < 4 then three_of_a_kind hand
  else
    let sorted = ranks_lst hand in
    let rec four_kind_help sorted =
      match sorted with
      | r1 :: r2 :: r3 :: r4 :: t -> begin
        if r1 = r2 && r2 = r3 && r3 = r4
          then Some (8, hand)
        else four_kind_help (r2 :: r3 :: r4 :: t)
      end
      | _ -> None
    in
    four_kind_help sorted

(** [straight_flush hand] is a tuple option with 9 and the list of ranks
    in descending order if the hand is a straight flush. If it is not, then it
    is None*)
let straight_flush hand =
  if straight hand != None && flush hand != None then
    Some (9, hand)
  else None

(** [royal_flush hand] is a tuple option with 10 and the list of cards
    in descending order. If no royal flush, then it is None*)
let royal_flush hand =
  let sorted = ranks_lst hand in
  if straight_flush hand = None then None
  else
    match sorted with
    | [r1; r2; r3; r4; r5] -> begin
      if r1 = 10 && r2 = 11 && r3 = 12 && r4 = 13 && r5 = 14
        then Some (10, hand) else None
    end
    | [r1; r2] -> begin
      if r1 = 10 && r2 = 11 || r1 = 11 && r2 = 12 || r1 = 12 && r2 = 13 ||
        r1 = 13 && r2 = 14
        then Some (10, hand)
      else None
    end
    | _ -> None

(** [combnk k lst] is all combinations of length k in list [lst]. Credit for
    the algorithm and implementation is given to 
    https://codereview.stackexchange.com/questions/40366/combinations-of-size-k-from-a-list-in-ocaml*)
let rec combnk k lst =
  if k <= 0 then [[]]
  else
    match lst with
    | [] -> []
    | h :: t -> begin
      let with_h = List.map (fun x -> h :: x) (combnk (k - 1) t) in
      let without_h = combnk k t in
      with_h @ without_h
    end

(** [every_hand pl st] is all the possible 5 hand combinations for player [pl] in
    state [st]. *)
let every_hand (pl : Player.player) (st : State.t) =
  let pl_two_cards = [ fst pl.hand; snd pl.hand ] in
  let table = st.table in
  match table.board with
  | Some [ c1; c2; c3; c4; c5 ] ->
      combnk 5 ([ c1; c2; c3; c4; c5 ] @ pl_two_cards)
  | _ -> raise (Invalid_argument "table is not filled completely")

(** [best_hand hand] is a tuple option with the value of best hand from
    a given card list [hand] and the list of that hand in descending order. *)
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

(** [remaining_boards table] are all remaining cards that can
    complete the board. *)
let remaining_boards board deck =
  let len = List.length board in
  if len = 0 then combnk 5 deck
  else if len = 3 then combnk 2 deck
  else if len = 4 then combnk 1 deck
  else []

let rank cards =
  let best = best_hand cards in
  match best with
  | None -> failwith "Impossible"
  | Some (rank, lst) -> rank

(***************************************************************************)
(* This poker probability algorithm is taken from the algorithm created by
  Billings, Papp, Schaeffer, and Szafron. Source: 
  https://en.wikipedia.org/wiki/Poker_Effective_Hand_Strength_(EHS)_algorithm*)
(***************************************************************************)

(** [hand_strength hand board deck] is the current strength of the hand
    given the current state of the hand, board, and deck*)
let hand_strength hand board deck =
  let current_rank = rank (hand @ board) in
  let poss_opp = combnk 2 deck in
  let (ahead, tied, behind) = 
    let rec strength_help ai_rank opp board ahead tied behind =
      match opp with
      | [] -> (ahead, tied, behind)
      | h :: t ->
        let opp_rank = rank (h @ board) in
        if (ai_rank > opp_rank)
          then strength_help ai_rank t board (ahead +. 1.0) tied behind
        else if (ai_rank = opp_rank)
          then strength_help ai_rank t board ahead (tied +. 1.0) behind
        else
          strength_help ai_rank t board ahead tied (behind +. 1.0)
    in strength_help current_rank poss_opp board 1.0 1.0 1.0
  in (ahead +. (tied /. 2.0)) /. (ahead +. tied +. behind)

(** Types x and current are used as helpers in the hand_potential function. *)
type x = {
  ahead : float;
  tied : float;
  behind : float
}
type current = {
  ahead_curr : x;
  tied_curr : x;
  behind_curr : x
}

(** [hand_potential ai_hand board deck] is a tuple, the first element representing
    the positive potential for the bot in the hand the second element
    representing the negative potential in the hand. *)
let hand_potential ai_hand board deck table =
  let ai_rank = rank (ai_hand @ board) in
  let opp_cards = combnk 2 deck in
  let hp_init_help = {ahead = 1.0; tied = 1.0; behind = 1.0} in
  let init_hp = {
    ahead_curr = hp_init_help;
    tied_curr = hp_init_help;
    behind_curr = hp_init_help
  } in
  let (total_hp, hp) =
    let rec hp_help1 ai_hand ai_rank opp_cards board deck total_hp hp =
      match opp_cards with
      | [] -> (total_hp, hp)
      | h :: t -> begin
        let opp_rank = rank (h @ board) in
        let new_total_hp =
          if (ai_rank > opp_rank)
            then {total_hp with ahead = (total_hp.ahead +. 1.0)}
          else if (ai_rank = opp_rank)
            then {total_hp with tied = (total_hp.tied +. 1.0)}
          else
            {total_hp with behind = (total_hp.behind +. 1.0)}
        in
        let now =
          if (ai_rank > opp_rank) then 0
          else if (ai_rank = opp_rank) then 1
          else 2
        in
        let board_combinations = remaining_boards board deck in
        let new_hand_pot =
          let rec hp_help2 ai_hand opp_cards board total_hp hp board_combos now =
            match board_combos with
            | [] -> hp
            | combo1 :: t' -> 
              let board1 = board @ combo1 in
              let ai_best = rank (ai_hand @ board1) in
              let opp_best = rank (opp_cards @ board1) in
              let new_hand_pot' =
                if now = 0 then
                  if (ai_best > opp_best)
                    then {hp with ahead_curr = ({hp.ahead_curr
                    with ahead = (hp.ahead_curr.ahead +. 1.0)})}
                  else if (ai_best = opp_best)
                    then {hp with ahead_curr = ({hp.ahead_curr
                    with tied = (hp.ahead_curr.tied +. 1.0)})}
                  else
                    {hp with ahead_curr = ({hp.ahead_curr
                    with behind = (hp.ahead_curr.behind +. 1.0)})}
                else if now = 1 then
                  if (ai_best > opp_best)
                    then {hp with tied_curr = ({hp.tied_curr
                    with ahead = (hp.tied_curr.ahead +. 1.0)})}
                  else if (ai_best = opp_best)
                    then {hp with tied_curr = ({hp.tied_curr
                    with tied = (hp.tied_curr.tied +. 1.0)})}
                  else
                    {hp with tied_curr = ({hp.tied_curr
                    with behind = (hp.tied_curr.behind +. 1.0)})}
                else
                  if (ai_best > opp_best)
                    then {hp with behind_curr = ({hp.behind_curr
                    with ahead = (hp.behind_curr.ahead +. 1.0)})}
                  else if (ai_best = opp_best)
                    then {hp with behind_curr = ({hp.behind_curr
                    with tied = (hp.behind_curr.tied +. 1.0)})}
                  else
                    {hp with behind_curr = ({hp.behind_curr
                    with behind = (hp.behind_curr.behind +. 1.0)})}
              in
              hp_help2 ai_hand opp_cards board total_hp new_hand_pot' t' now
            in
          hp_help2 ai_hand h board total_hp hp board_combinations now
        in
        hp_help1 ai_hand ai_rank t board deck new_total_hp new_hand_pot
      end
      in
      hp_help1 ai_hand ai_rank opp_cards board deck hp_init_help init_hp
    in
    let pos_potential = (hp.behind_curr.ahead +. (hp.behind_curr.tied /. 2.0)
      +. (hp.tied_curr.ahead /. 2.0)) /. (total_hp.behind +. total_hp.tied) in
    let neg_potential = (hp.ahead_curr.behind +. (hp.tied_curr.behind /. 2.0)
      +. (hp.ahead_curr.tied /. 2.0)) /. (total_hp.ahead +. total_hp.tied) in
    (pos_potential, neg_potential)

(** [ehs ai_hand board deck] is the effective hand strength of the hand
    given the board and deck, using the algorithm created above. *)
let ehs ai_hand board deck table =
  let hs = hand_strength ai_hand board deck in
  let (pPot, nPot) = hand_potential ai_hand board deck table in
  hs *. (1. -. nPot) +. (1. -. hs) *. pPot

(** [easy_bot st] is the command of the easy ai. *)
let easy_bot st =
  let random = Random.int 10 in
  if st.active_bet = 0
    then if random < 5
      then Check else Bet 10
  else
    if random < 5
      then Fold else Call

(** [conservative st] is the conservative command of the hard ai. *)
let conservative st =
  let random = Random.int 10 in
  if st.active_bet = 0
    then Check
  else
    if random < 3
      then Fold else Call

(** [aggressive st ai] is the aggressive command of the hard ai,
    given the state and player [ai]. *)
let aggressive st ai =
  let random = Random.int 10 in
  let half_pot = st.pot / 2 in
  let bet_unit = if half_pot > ai.stack then ai.stack else half_pot in
  let raise_unit = if bet_unit + st.active_bet > ai.stack
    then ai.stack - st.active_bet else bet_unit in
  if st.active_bet = 0
    then if random < 3
      then Check else Bet bet_unit
  else
    if random < 3
      then Call else Raise raise_unit

(** [basic st] is the basic move for the ai: check or call. *)
let basic st =
  if st.active_bet = 0 then Check else Call

(** sublist algorithm taken from stack exchange:
    https://stackoverflow.com/questions/2710233/how-to-get-a-sub-list-from-a-list-in-ocaml. *)
let sublist lst =
  let rec sublist_help b e l = 
    match l with
      [] -> failwith "sublist"
    | h :: t -> 
        let tail = if e=0 then [] else sublist_help (b-1) (e-1) t in
        if b>0 then tail else h :: tail
  in
  sublist_help 0 (List.length lst / 10) lst

(** [get_first_bot pls] is the first bot in the player list [pls]. *)
let rec get_first_bot pls =
  match pls with
  | [] -> failwith "Impossible"
  | h :: t -> if h.is_AI then h else get_first_bot t

(** [option_to_lst opt] is the list form of a list option [opt]. *)
let option_to_lst opt =
  match opt with
  | None -> []
  | Some x -> x

(** [tuple_to_lst tup] is the list form of a tuple pair [tup]. *)
let tuple_to_lst tup =
  match tup with
  | (a1, a2) -> [a1; a2]

(** [hard_bot st] is the command of the hard ai bot given state [st]. *)
let hard_bot st =
  let ai = get_first_bot st.players in
  let ai_hand = tuple_to_lst (ai.hand) in
  let table = st.table in
  let board = option_to_lst (table.board) in
  let deck = sublist (table.deck) in
  let ehs = ehs ai_hand board deck table in
  if ehs < 0.2 then conservative st
  else if ehs > 0.7 then aggressive st ai
  else basic st

(** [command st] is the command given by the bot given state [st]. *)
let command st =
  if st.ai_difficulty = "easy"
    then easy_bot st
  else hard_bot st