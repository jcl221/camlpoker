(** The type of a guest. Stores the following information: the guest's
    player id, the total chips they have, their bet amount, their hand
    at the table, and finally whether they've forfeited the match. *)
type guest = {
  id : string;
  mutable hand : (Card.t * Card.t) option;
  mutable chips : int;
  mutable bet : int;
  mutable forfeited : bool;
}

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
  players : guest list;
  mutable action_queue : guest list;
  mutable active_bet : int;
  mutable table : Table.table;
  mutable pot : int;
  mutable winner : guest option;
}

exception Empty_Hand

let init_state ids =
  let starting_table = Table.init_table () in
  let init_player id =
    {
      id;
      chips = 50;
      bet = 0;
      hand = Some (Table.deal_hand starting_table);
      forfeited = false;
    }
  in
  {
    players = List.map init_player ids;
    action_queue = [];
    active_bet = 0;
    table = starting_table;
    pot = 0;
    winner = None;
  }

(** [get_player st id] is the active player with id [id] for game state
    [st].
    Raises: Not_found if there is no player with id [id] in the given 
    state.*)
let get_player st id = st.players |> List.find (fun x -> x.id = id)

let has_forfeited st id =
  let player = get_player st id in
  player.forfeited

let string_of_hand st id =
  let player = get_player st id in
  match player.hand with
  | None -> "None"
  | Some (c1, c2) ->
      let c1_string = Card.string_of_card c1 in
      let c2_string = Card.string_of_card c2 in
      "( " ^ c1_string ^ ", " ^ c2_string ^ " )"

let player_info st id =
  let player = get_player st id in
  let c = string_of_int player.chips in
  let b = string_of_int player.bet in
  player.id ^ " {chips: " ^ c ^ "; bet: " ^ b ^ "}"

let string_of_table st =
  let board = st.table.board in
  match board with
  | None -> "None"
  | Some lst -> Util.string_of_list Card.string_of_card lst

let deal_center st = st.table <- Table.place_center st.table

let fold st id =
  let p = get_player st id in
  p.forfeited <- true

let bet st id amt =
  let p = get_player st id in
  p.bet <- p.bet + amt;
  st.active_bet <- amt;
  st.pot <- st.pot + amt

let add_turns st =
  let enqueue_player p =
    if not p.forfeited then st.action_queue <- st.action_queue @ [ p ]
  in
  List.iter enqueue_player st.players

let get_turn st =
  match st.action_queue with
  | x :: xs ->
      st.action_queue <- xs;
      Some x.id
  | [] -> None

let active_bet st = st.active_bet

let perform_turn st id cmd = failwith "Unimplemented"

let player_hands st = failwith "Unimplemented"

let showdown st = failwith "Unimplemented"
