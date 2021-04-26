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
let init_state ids = failwith "Unimplemented"

let get_player st id = failwith "Unimplemented"

(**st.table <- Table.place_center st.table*)
let deal_center st = failwith "Unimplemented"

(**let p = get_player st id in
  p.forfeited <- true*)
let fold st id = failwith "Unimplemented"

let bet st id amt =
  let survey (p : Player.player) =
    if p.name = id then { p with stack = p.stack - amt } else p
  in
  let updated_players = List.map survey st.players in
  { st with players = updated_players; pot = st.pot + amt }

(** st |> player_hands |> Table.ranker (outputs a player) 
  The player list needs to be reset and used to initialize the next game:
  
  You now need to update the player in the [players] list to give that guy 
  all the winnings, unfold all the folded players, then initialize a new state
  with that list and with a new pot, then maybe print out a winner? 
  
  player_init should take in an argument for setting the player's # of chips. *)
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
