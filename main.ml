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

(** The queue for players pending an action during a betting round. Each
    element of the queue stores the name of a player and their current
    bet in the betting round. *)
let action_queue : (string * int) Queue.t = Queue.create ()

(** The queue for players that have finished performing their required
    action during a betting round and still have stakes in the game. *)
let finished_queue : (string * int) Queue.t = Queue.create ()

(** [reset_queues] clears [action_queue] and [finished_queue]. *)
let reset_queues () =
  Queue.clear action_queue;
  Queue.clear finished_queue

(** [add_turns players] enqueues the players with names listed in
    [players] onto the action queue. *)
let add_turns players =
  List.iter (fun name -> Queue.push (name, 0) action_queue) players

(** [restore_turns ()] enqueues all of the players in [finished_queue]
    back onto [action_queue]. *)
let restore_turns () = Queue.transfer finished_queue action_queue

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
  | Check -> cmd
  | Bet amt -> cmd
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

(** [get_command name st] is the command for a valid poker action given
    by player of name [name] in game state [st]. *)
let get_command name st =
  let is_dummy = (State.get_player name st).is_AI in
  if is_dummy then Ai.command st
  else if State.active_bet st = 0 then prompt_cmd_init name
  else prompt_cmd_open name

(** [make_bet name current_bet raise_to st] is the next game state from
    [st] after a player with name [name] and a bet of [current_bet] in
    the current betting round raises their bet to [raise_to]. Updates
    action/finished queues accordingly. *)
let make_bet name current_bet raise_to st =
  let active_bet = State.active_bet st in
  assert (raise_to > current_bet && raise_to >= active_bet);

  if raise_to > active_bet then restore_turns ();
  Queue.push (name, raise_to) finished_queue;
  State.bet (name, current_bet) raise_to st

(** [turn st] is the next state from [st] all players in the action
    queue have performed their required turns. *)
let rec perform_turns st =
  try
    let name, bet = Queue.pop action_queue in
    let cmd = get_command name st in
    let st' =
      match cmd with
      | Bet amt -> make_bet name 0 amt st
      | Raise amt -> make_bet name bet amt st
      | Call -> make_bet name bet (State.active_bet st) st
      | Fold -> State.fold name st
      | Check ->
          Queue.push (name, bet) finished_queue;
          st
      | _ -> failwith "invalid command parsed"
    in
    perform_turns st'
  with Queue.Empty -> st

(** [betting_round st players] is the updated state from initial state
    [st] after a betting round has occurred. Specifically, it is the
    state after all players with ids listed in [players] have performed
    an action upon being prompted to do so. *)
let betting_round st =
  reset_queues ();
  st |> State.active_players |> add_turns;
  perform_turns st

(************************************************************************)

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
let draw main_user st = State.print_state st main_user false

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
