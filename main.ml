(**let betting_round st =
  State.add_turns st;
  let rec player_turns () =
    let active_bet = State.active_bet st in
    match State.get_turn st with
    | None -> ()
    | Some id ->
        (let cmd = read_line () in
         if active_bet = 0 then
           match cmd with
           | "bet" ->
               print_endline "Please enter your bet amount";
               let amt = read_line () |> int_of_string in
               State.bet st id amt;
               print_endline ("You bet: " ^ string_of_int amt)
           | "check" -> ()
           | _ -> print_endline "invalid command"
         else
           match cmd with
           | "raise" ->
               print_endline "Please enter your bet amount";
               let amt = read_line () |> int_of_string in
               State.bet st id (active_bet + amt)
           | "fold" -> State.fold st id
           | _ -> print_endline "invalid command");

        player_turns ()
  in
  player_turns () *)

module Command = struct
  type t =
    | Bet of int
    | Raise of int
    | Check
    | Fold
    | Invalid

  let parse str =
    let words =
      str |> String.lowercase_ascii
      |> String.split_on_char ' '
      |> List.filter (fun x -> x <> "")
    in
    match words with
    | [ x ] when x = "fold" -> Fold
    | [ x ] when x = "check" -> Check
    | [ x1; x2 ] when x1 = "bet" -> (
        try
          let amt = int_of_string x2 in
          Bet amt
        with Failure _ -> Invalid)
    | [ x1; x2 ] when x1 = "raise" -> (
        try
          let amt = int_of_string x2 in
          Raise amt
        with Failure _ -> Invalid)
    | _ -> Invalid
end

(** [prompt_command id] is the command inputted into the command line by 
  the user after being prompted to perform an action.  *)
let rec prompt_command id =
  print_endline (id ^ {|'s Turn: please enter a command |});
  let cmd = read_line () |> Command.parse in
  match cmd with
  | Invalid ->
      print_endline "Invalid command. Please try again.";
      prompt_command id
  | x -> x

(** [betting_round st players] is the updated state from initial state [st] 
    after a betting round has occurred. Specifically, it is the state after 
    all players with ids listed in [players] have performed an action upon 
    being prompted to do so. *)
let rec betting_round st players =
  match players with
  | [] -> st
  | id :: t ->
      let st' =
        match prompt_command id with
        | Bet x -> State.bet st id x
        | Raise x -> State.active_bet st + x |> State.bet st id
        | Fold -> State.fold st id
        | Check -> st
        | _ -> failwith "invalid command parsed"
      in
      betting_round st' t

(** [print_player st user_id id] prints the relevant game state information 
    for player with id [id] in state [st]. 
    Only the player with [user_id] (i.e., the main user) will have their hand 
    displayed. *)
let print_player st user_id id =
  let profile = Player.player_info id in
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
  print_endline ("Community Cards: " ^ Table.string_of_table st);
  print_endline "\n*** Players ***";
  List.iter (print_player st user_id) lobby

(** Starts a new camlpoker match. *)
let play user_id =
  let lobby = [ user_id; "Bot1"; "Bot2" ] in
  let state = State.init_state lobby in
  for i = 0 to 2 do
    State.deal_center state
  done;
  draw state lobby user_id;
  betting_round state;
  State.deal_center state;
  print_endline ("community cards: " ^ State.string_of_table state);
  betting_round state;
  State.deal_center state;
  print_endline ("community cards: " ^ State.string_of_table state);
  betting_round state;
  print_endline "Showdown!"

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
