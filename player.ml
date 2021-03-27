type player = {
  name : string;
  hand : string list; (*This should be Card.t list but its not working*)
}

let player_init name hand = { name; hand }

(*This function is from A2 (was provided to us)*)
let pp_string s = "\"" ^ s ^ "\""

(*This function is from A2 (was provided to us)*)
let pp_list pp_elt lst =
  let pp_elts lst =
    let rec loop n acc = function
      | [] -> acc
      | [ h ] -> acc ^ pp_elt h
      | h1 :: (h2 :: t as t') ->
          if n = 100 then acc ^ "..." (* stop printing long list *)
          else loop (n + 1) (acc ^ pp_elt h1 ^ "; ") t'
    in
    loop 0 "" lst
  in
  "[" ^ pp_elts lst ^ "]"

let hand_to_string player hand =
  let hand_to_print = player.hand in
  pp_list pp_string hand_to_print
