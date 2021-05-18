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
