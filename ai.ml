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
let suits_lst hand = List.map (fun (x : Card.t) -> x.suit) hand

(** [highest_rank hand] is the highest rank of the cards in [hand]*)
let highest_rank hand =
  let ranks = ranks_lst hand in
  match ranks with
  | [ _; _; _; _; r5 ] -> r5
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
  | [ r1; r2; r3; r4; r5 ] ->
      if r1 = r2 && r2 = r3 && r3 = r4 then
        Some (8, [ r1; r2; r3; r4; r5 ])
      else if r2 = r3 && r3 = r4 && r4 = r5 then
        Some (8, [ r2; r3; r4; r5; r1 ])
      else None
  | _ -> raise (Invalid_argument "hand is invalid")

(** [three_of_a_kind hand] is a tuple option with 4 and the list of ranks with
    the three similar cards at the front and the two different ones at the
    end in descending order. If there is no three of a kind, it is None*)
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
    descending order if the hand is a straight. If it is not, then it is None*)
let straight hand =
  let ranks = ranks_lst hand in
  match ranks with
  | [ r1; r2; r3; r4; r5 ] ->
      if r1 = r2 - 1 && r2 = r3 - 1 - 1 && r4 = r5 - 1 then
        Some (5, [ r5; r4; r3; r2; r1 ])
      else if r1 = 2 && r2 = 3 && r3 = 4 && r4 = 5 && r5 = 14 then
        Some (5, [ r4; r3; r2; r1; r5 ])
      else None
  | _ -> raise (Invalid_argument "hand is invalid")

(** [flush hand] is a tuple option with 6 and the list of ranks in
    descending order if the hand is a flush. If it is not, then it is None*)
let flush hand =
  let suits = suits_lst hand in
  match suits with
  | [ s1; s2; s3; s4; s5 ] ->
      if s1 = s2 && s2 = s3 && s3 = s4 && s4 = s5 then
        Some (6, List.rev (ranks_lst hand))
      else None
  | _ -> raise (Invalid_argument "hand is invalid")

(** [straight_flush hand] is a tuple option with 9 and the list of ranks
    in descending order if the hand is a straight flush. If it is not, then it
    is None*)
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

(** [full_house hand] is a tuple option with 7 and the list of ranks with
    the three of a kind in the front and the pair in the back. If no full
    house, then it is None*)
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
    pairs at the leftmost of the list in descending order and the different
    caard at the end*)
let two_pair_help values ranks =
  let first_four = List.rev (List.sort compare values) in
  first_four
  @ List.filter (fun x -> List.mem x first_four = false) ranks

(** [two_pair hand] is a tuple option with 3 and a list of ranks so that
    the pairs are on the left in descending order. If there is no two pair,
    then it is None. *)
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

(** [combnk k lst] is all combinations of length k in list [lst]. Credit for
    the algorithm and implementation is given to 
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
let remaining_boards (table : Table.table) =
  let board = table.board in
  let deck = table.deck in
  match board with
  | None -> combnk 5 deck
  | Some cards ->
      let len = List.length cards in
      if len = 3 then combnk 2 deck
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
        let board_combinations = remaining_boards table in
        let new_hand_pot =
          let rec hp_help2 ai_hand opp_cards board total_hp hp board_combos index =
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

let command st = failwith "Unimplemented"
