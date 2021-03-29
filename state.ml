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

(** AF: A record {active = a; forfeited = f; center = c; pot = p; winner
    = w } is the game state with active players [a], forfeited players
    [f], center cards [c], a pot of amount [p], and a winner [w].

    RI: * [active] and [forfeited] each do not store duplicate guests. *
    [active] does not share any guests with [forfeited]. * [center] has
    5 elements. * [winner] is an existing guest in [active]. *)
type t = {
  players : guest list;
  mutable action_queue : guest list;
  mutable table : Table.table;
  mutable pot : int;
  mutable winner : guest option;
}

let init_state ids =
  let init_player id =
    { id; chips = 50; bet = 0; hand = None; forfeited = false }
  in
  {
    players = List.map init_player ids;
    action_queue = [];
    table = Table.init_table ();
    pot = 0;
    winner = None;
  }

let deal st =
  let deck = fst st.table in
  let deal_hand gst = gst.hand <- Table.deal_one_hand deck in
  List.iter deal_hand st.players;
  for i = 0 to 2 do
    Table.new_card st.table
  done

(** [get_player st id] is the active player with id [id] for game state
    [st]. *)
let get_player st id = st.players |> List.find (fun x -> x.id = id)

let fold st id =
  let player = get_player st id in
  player.forfeited <- true

let bet st id amt =
  let player = get_player st id in
  player.bet <- player.bet + amt

let get_hands st = failwith "Unimplemented"

let showdown st = failwith "Unimplemented"
