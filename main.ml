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

(** [prompt_cmd_init name] prompts the player with name [name] for a
    valid command when a betting round has just started. Returns the
    corresponding command entered if it is either [Bet] or [Check],
    otherwise re-prompts the player. *)
let rec prompt_cmd_init name =
  let hint = "('bet <amt>' or 'check')" in
  let msg = name ^ "'s Turn: enter a command " ^ hint in
  let cmd = msg |> prompt |> Command.parse in
  match cmd with
  | Bet _ | Check -> cmd
  | _ ->
      print_endline ("Invalid action. " ^ hint);
      prompt_cmd_init name

(** [prompt_cmd_open name] prompts the player with name [name] for a
    valid command after a betting round has been opened. Returns the
    corresponding command entered if it is either [Raise], [Call], or
    [Fold], otherwise re-prompts the player. *)
let rec prompt_cmd_open name =
  let hint = "('raise <amt>', 'call', or 'fold')" in
  let msg = name ^ "'s Turn: enter a command: " ^ hint in
  let cmd = msg |> prompt |> Command.parse in
  match cmd with
  | Raise _ | Call | Fold -> cmd
  | _ ->
      print_endline ("Invalid action. " ^ hint);
      prompt_cmd_open name

(** [prompt_command id st] is the command entered into the command line
    after prompting the player with name [name] to do so. Only accepts
    actions appropriate to the current game state [st], re-prompting the
    player as needed if an invalid command is given. *)
let prompt_command name st =
  if State.active_bet st = 0 then prompt_cmd_init name
  else prompt_cmd_open name

let rec combine_list lst1 =
  match lst1 with [] -> [] | h :: t -> (h, 0) :: combine_list t

let rec all_bets_equal pls highest =
  match pls with
  | [] -> true
  | (id, bet) :: t -> highest = bet && all_bets_equal t highest

(** [betting_round st players] is the updated state from initial state
    [st] after a betting round has occurred. Specifically, it is the
    state after all players with ids listed in [players] have performed
    an action upon being prompted to do so. *)
let betting_round st =
  let rec betting_aux players st highest num_checks =
    if
      all_bets_equal players highest
      || num_checks = List.length (State.active_players st)
    then st
    else
      match players with
      | [] -> st
      | (id, bet) :: t -> (
          let cmd = prompt_command id st in
          match cmd with
          | Bet x ->
              let st' = State.bet (id, bet) x st in
              betting_aux (t @ [ (id, x) ]) st' x num_checks
          | Check ->
              let st' = st in
              betting_aux
                (t @ [ (id, bet) ])
                st' highest (num_checks + 1)
          | Raise x ->
              let st' = State.bet (id, bet) x st in
              betting_aux
                (t @ [ (id, x) ])
                st' (State.active_bet st') num_checks
          | Call ->
              let st' = State.bet (id, bet) (State.active_bet st) st in
              betting_aux
                (t @ [ (id, State.active_bet st) ])
                st' highest num_checks
          | Fold ->
              let st' = State.fold id st in
              betting_aux t st' highest num_checks
          | _ -> failwith "invalid command parsed")
  in
  betting_aux (combine_list (State.active_players st)) st 1 0

(** [update st] is the new game state after the poker match in state
    [st] progresses through one betting round and the table is updated
    accordingly. *)
let update st =
  let post_bet = betting_round st in
  match State.active_players post_bet with
  | [] -> failwith "impossible"
  | [ _ ] as winners -> State.reset winners st
  | _ -> (
      match State.stage_of_game st with
      | Preflop -> post_bet |> State.deal_center 3
      | Midgame -> post_bet |> State.deal_center 1
      | Showdown -> post_bet |> State.showdown)

(** [draw st player_id] draws the game state [st] onto the UI. The hands
    of every player except the main user (identified by the id
    [main_user]) are obscured. *)
let draw main_user st = State.print_state st main_user

(** [game_loop main_user st] draws the state [st] onto the UI and
    updates it accordingly for another iteration of loop. The player
    with id [main_user] is assumed to be the main user. *)
let rec game_loop main_user st =
  draw main_user st;
  st |> update |> game_loop main_user

(** Greets the player, prompts them for a name, then starts the main
    game loop. *)
let main () =
  ANSITerminal.print_string [ ANSITerminal.green ]
    "\n\nWelcome to CamlPoker.\n";
  let user_id = prompt "Please enter a player id:" in
  let difficulty =
    prompt
      "Would you like an easy or hard opponent? (Please enter 'easy' \
       or 'hard')"
  in
  let state = State.init_state [ user_id; "Dummy" ] in
  { state with ai_difficulty = difficulty } |> game_loop user_id

let () = main ()
