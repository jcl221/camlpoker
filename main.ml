(** Starts a new poker match. *)
let play pname = print_endline "Your cards: "

(** [prompt message] is the user input entered in response to a
    [message] printed onto stdout. *)
let prompt message =
  print_endline message;
  read_line ()

(** Greets the player, prompts them for a name, then starts the main
    game. *)
let main () =
  ANSITerminal.print_string [ ANSITerminal.red ]
    "\n\nWelcome to CamlPoker.\n";
  let welcome = "Please enter your player name:\n>>" in
  welcome |> prompt |> play

let () = main ()
