type t =
  | Bet of int
  | Check
  | Raise of int
  | Call
  | Fold
  | Invalid

let parse str =
  let keywords =
    str |> String.lowercase_ascii
    |> String.split_on_char ' '
    |> List.filter (fun x -> x <> "")
  in
  match keywords with
  | [ "bet"; x2 ] -> (
      try Bet (int_of_string x2) with Failure _ -> Invalid)
  | [ "raise"; x2 ] -> (
      try Raise (int_of_string x2) with Failure _ -> Invalid)
  | [ "check" ] -> Check
  | [ "call" ] -> Call
  | [ "fold" ] -> Fold
  | _ -> Invalid

let string_of_cmd cmd =
  match cmd with
  | Bet amt -> "Bet " ^ string_of_int amt
  | Raise amt -> "Raise to " ^ string_of_int amt
  | Check -> "Check"
  | Call -> "Call"
  | Fold -> "Fold"
  | Invalid -> "Invalid"
