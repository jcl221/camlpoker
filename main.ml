module Command = struct
  type t =
    | Bet of int
    | Check
    | Raise of int
    | Call
    | Fold
    | Invalid

  (** [parse str] is the command corresponding to [str]. If [str] doesn't 
      correspond to any valid poker action, [Invalid] is returned. *)
  let parse str =
    let keywords =
      str |> String.lowercase_ascii
      |> String.split_on_char ' '
      |> List.filter (fun x -> x <> "")
    in
    match keywords with
    | [ "call" ] -> Call
    | [ "fold" ] -> Fold
    | [ "check" ] -> Check
    | [ "bet"; x2 ] -> (
        try
          let amt = int_of_string x2 in
          Bet amt
        with Failure _ -> Invalid)
    | [ "raise"; x2 ] -> (
        try
          let amt = int_of_string x2 in
          Raise amt
        with Failure _ -> Invalid)
    | _ -> Invalid
end

module Opponent = struct
  let turn st : Command.t =
    print_string "Dummy's Turn: ";
    match State.active_bet st with
    | 0 ->
        print_endline "check";
        Check
    | _ ->
        print_endline "call";
        Call
end

(** [prompt message] is the user input entered in response to a
    [message] printed onto stdout. *)
let prompt message =
  print_endline message;
  print_string ">> ";
  read_line ()

(** [prompt_command id] is the command entered into the command line by 
    the user after being prompted to perform an action. *)
let rec prompt_command id st =
  let msg_base = id ^ "'s Turn: please enter a command " in
  if State.active_bet st = 0 then
    let msg = msg_base ^ "('bet <amt>' or 'check')" in
    let cmd = msg |> prompt |> Command.parse in
    match cmd with
    | Invalid | Raise _ | Call | Fold ->
        print_endline "Invalid. Please enter 'bet <amt>' or 'check'. \n";
        prompt_command id st
    | x -> x
  else
    let msg = msg_base ^ "('raise <amt>', 'call', or 'fold')" in
    let cmd = msg |> prompt |> Command.parse in
    match cmd with
    | Invalid | Bet _ | Check ->
        print_endline
          "Invalid. Please enter 'raise <amt>', 'call', or 'fold'.";
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
          let cmd =
            if (State.get_player id st).is_AI then Opponent.turn st
            else prompt_command id st
          in
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

(** [update st] is the new game state after the poker match in state [st] 
    progresses through one betting round and the table is updated 
    accordingly. *)
let update st =
  let post_bet = betting_round st in
  match State.stage_of_game st with
  | Preflop -> post_bet |> State.deal_center 3
  | Midgame -> post_bet |> State.deal_center 1
  | Showdown -> post_bet |> State.showdown

(** [draw st player_id] draws the game state [st] onto the UI.
    The hands of every player except the main user (identified by 
    the id [main_user]) are obscured. *)
let draw main_user st = State.print_state st main_user

(** [game_loop main_user st] draws the state [st] onto the UI and updates 
    it accordingly for another iteration of loop. The player with 
    id [main_user] is assumed to be the main user. *)
let rec game_loop main_user st =
  draw main_user st;
  st |> update |> game_loop main_user

(** Greets the player, prompts them for a name, then starts the main
    game loop. *)
let main () =
  ANSITerminal.print_string [ ANSITerminal.green ]
    "\n\nWelcome to CamlPoker.\n";
  let user_id = prompt "Please enter a player id:" in
  [ user_id; "Dummy" ] |> State.init_state |> game_loop user_id

let () = main ()
