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

let init_state ids = failwith "Unimplemented"
  (**let starting_table = Table.init_table () in
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
  }*)

let deal_center st = failwith "Unimplemented"
  (**st.table <- Table.place_center st.table*)

let fold st id = failwith "Unimplemented"
  (**let p = get_player st id in
  p.forfeited <- true*)

let bet st id amt = failwith "Unimplemented"
  (**let p = get_player st id in
  p.bet <- p.bet + amt;
  st.active_bet <- amt;
  st.pot <- st.pot + amt*)

let showdown st = failwith "Unimplemented"
let active_bet st = st.active_bet

let get_player st id =
  let rec get_player_from_lst (lst : Player.player list) id =
    match lst with
    | [] -> raise (Failure "Not Found")
    | h :: t -> begin
      if h.name = id then h else get_player_from_lst t id
    end
  in
  get_player_from_lst st.players id

let player_hand (player : Player.player) =
  (player.name, player.hand)
let player_hands st =
  List.map player_hand st.players

