module Command = struct
  type t =
    | Bet of int
    | Check
    | Raise of int
    | Call
    | Fold
    | Invalid

  let parse str =
    let clauses =
      str |> String.lowercase_ascii
      |> String.split_on_char ' '
      |> List.filter (fun x -> x <> "")
    in
    match clauses with
    | [ x ] when x = "call" -> Call
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

(** [prompt message] is the user input entered in response to a
    [message] printed onto stdout. *)
let prompt message =
  print_endline message;
  print_string ">> ";
  read_line ()

(** [prompt_command id] is the command inputted into the command line by 
  the user after being prompted to perform an action. *)
let rec prompt_command id st =
  let msg = id ^ {|'s Turn: please enter a command |} in
  let cmd = msg |> prompt |> Command.parse in
  if State.active_bet st = 0 then
    match cmd with
    | Invalid | Raise _ | Call | Fold ->
        print_endline "Invalid. Please enter 'bet x' or 'check'.";
        prompt_command id st
    | x -> x
  else
    match cmd with
    | Invalid | Bet _ | Check ->
        print_endline
          "Invalid. Please enter 'raise x', 'call', or 'fold'.";
        prompt_command id st
    | x -> x

(** [betting_round st players] is the updated state from initial state [st] 
    after a betting round has occurred. Specifically, it is the state after 
    all players with ids listed in [players] have performed an action upon 
    being prompted to do so. *)
let betting_round st =
  let rec betting_aux players st =
    match players with
    | [] -> st
    | id :: t ->
        let st' =
          let cmd = prompt_command id st in
          match cmd with
          | Bet x -> State.bet id x st
          | Check -> st
          | Raise x -> State.bet id (x + State.active_bet st) st
          | Call -> State.bet id (State.active_bet st) st
          | Fold -> State.fold id st
          | _ -> failwith "invalid command parsed"
        in
        betting_aux t st'
  in
  betting_aux (State.ready_players st) st

let update st =
  let post_bet = betting_round st in
  match State.stage_of_game st with
  | Preflop -> post_bet |> State.deal_center 3
  | Midgame -> post_bet |> State.deal_center 1
  | Showdown -> post_bet |> State.showdown

(** [draw st player_id] draws the game state [st] onto the UI,
    assuming that the player with id [user_id] is the main user and 
    that the game lobby consists of players with ids listed in [lobby]. *)
let draw main_user st = State.print_state st main_user

let rec game_loop main_user st =
  draw main_user st;
  st |> update |> game_loop main_user

(** Greets the player, prompts them for a name, then starts the main
    game loop. *)
let main () =
  ANSITerminal.print_string [ ANSITerminal.green ]
    "\n\nWelcome to CamlPoker.\n";
  let user_id = prompt "Please enter a player id:" in
  [ user_id; "Bot 1"; "Bot 2" ] |> State.init_state |> game_loop user_id

let () = main ()
