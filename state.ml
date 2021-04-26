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

let bet st id amt = failwith "Unimplemented"

let showdown st = failwith "Unimplemented"

let active_bet st = st.active_bet

let get_player st id = failwith "Unimplemented"

let player_hands st = failwith "Unimplemented"
