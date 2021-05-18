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

let rec combine_list lst1 =
  match lst1 with
  | [] -> []
  | h :: t -> (h, 0) :: combine_list t

let rec all_bets_equal pls highest =
  match pls with
  | [] -> true
  | (id, bet) :: t -> (highest = bet) && all_bets_equal t highest

(** [betting_round st players] is the updated state from initial state [st] 
    after a betting round has occurred. Specifically, it is the state after 
    all players with ids listed in [players] have performed an action upon 
    being prompted to do so. *)
let betting_round st =
  let rec betting_aux players st highest num_checks =
    if all_bets_equal players highest ||
      num_checks = List.length (State.ready_players st)
      then st
    else begin
      match players with
      | [] -> st
      | (id, bet) :: t ->
          let cmd = prompt_command id st in
          match cmd with
          | Bet x -> begin
            let st' = State.bet id x st
            in betting_aux (t @ (id, x) :: []) st' x num_checks
          end
          | Check -> begin
            let st' = st in
            betting_aux (t @ (id, bet) :: []) st' highest (num_checks + 1)
          end
          | Raise x -> begin
            let st' = State.bet id (x + State.active_bet st) st
            in
            betting_aux (t @ (id, State.active_bet st') :: [])
            st' (State.active_bet st') num_checks
          end
          | Call -> begin
            let st' = State.bet id (State.active_bet st - bet) st
            in betting_aux (t @ (id, highest) :: [])
            st' highest num_checks
          end
          | Fold -> begin
            let st' = State.fold id st in
            betting_aux t st' highest num_checks
          end
          | _ -> failwith "invalid command parsed"
        end
  in
  betting_aux (combine_list (State.ready_players st)) st 1 0

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
