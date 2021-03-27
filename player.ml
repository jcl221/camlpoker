type hand_type = Card.t list

type player = {
  name : string;
  hand : hand_type;
}

let player_init name hand = { name; hand }

(*This function is from A2 (was provided to us). Not using it right now, ignore it*)
let pp_string s = "\"" ^ s ^ "\""

(*This function is from A2 (was provided to us). Not using it right now, ignore it*)
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

let rec string_helper lst acc =
  match lst with
  | [] -> acc
  | h :: t -> string_helper t (acc ^ Card.string_of_card h)

let hand_to_string player =
  let hand_to_print = player.hand in
  string_helper hand_to_print ""
