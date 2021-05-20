(** hi*)
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

let rec combine_list lst1 =
  match lst1 with [] -> [] | h :: t -> (h, 0) :: combine_list t

let rec all_bets_equal pls highest =
  match pls with
  | [] -> true
  | (id, bet) :: t -> highest = bet && all_bets_equal t highest

(** [betting_round st players] is the updated state from initial state [st] 
    after a betting round has occurred. Specifically, it is the state after 
    all players with ids listed in [players] have performed an action upon 
    being prompted to do so. *)
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
              let st' = State.bet id x st in
              betting_aux (t @ [ (id, x) ]) st' x num_checks
          | Check ->
              let st' = st in
              betting_aux
                (t @ [ (id, bet) ])
                st' highest (num_checks + 1)
          | Raise x ->
              let st' = State.bet id (x + State.active_bet st) st in
              betting_aux
                (t @ [ (id, State.active_bet st') ])
                st' (State.active_bet st') num_checks
          | Call ->
              let st' = State.bet id (State.active_bet st - bet) st in
              betting_aux (t @ [ (id, highest) ]) st' highest num_checks
          | Fold ->
              let st' = State.fold id st in
              betting_aux t st' highest num_checks
          | _ -> failwith "invalid command parsed")
  in
  betting_aux (combine_list (State.active_players st)) st 1 0

(** [update st] is the new game state after the poker match in state [st] 
    progresses through one betting round and the table is updated 
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
  let difficulty = prompt "Would you like an easy or hard opponent?" in
  let state = State.init_state [ user_id; "Dummy" ] in
  { state with ai_difficulty = difficulty } |> game_loop user_id

let () = main ()
