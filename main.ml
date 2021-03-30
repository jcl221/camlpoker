(** [print_player st user_id id] prints the relevant game state information 
    for player with id [id] in state [st]. 
    Only the player with [user_id] (i.e., the main user) will have their hand 
    displayed. *)
let print_player st user_id id =
  let profile = State.player_info st id in
  let cards =
    match State.has_forfeited st id with
    | true -> "Folded"
    | false ->
        if id = user_id then State.string_of_hand st id
        else "( Hidden, Hidden )"
  in
  print_endline (profile ^ ": " ^ cards)

(** [draw st player_id] draws the game state [st] onto the UI,
    assuming that the player with id [user_id] is the main user and 
    that the game lobby consists of players with ids listed in [lobby]. *)
let draw st lobby user_id =
  print_endline "\n*** Table ***";
  print_endline ("Community Cards: " ^ State.string_of_table st);
  print_endline "\n*** Players ***";
  List.iter (print_player st user_id) lobby

(** Starts a new camlpoker match. *)
let play user_id =
  let lobby = [ user_id; "Bot1"; "Bot2" ] in
  let state = State.init_state lobby in
  State.deal state;
  draw state lobby user_id

(** [prompt message] is the user input entered in response to a
    [message] printed onto stdout. *)
let prompt message =
  print_endline message;
  print_string ">> ";
  read_line ()

(** Greets the player, prompts them for a name, then starts the main
    game. *)
let main () =
  ANSITerminal.print_string [ ANSITerminal.green ]
    "\n\nWelcome to CamlPoker.\n";
  let welcome = "Please enter a player id:" in
  welcome |> prompt |> play

let () = main ()
