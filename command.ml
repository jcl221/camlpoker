type command =
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