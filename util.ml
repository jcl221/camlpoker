(** [string_of_list string_of_elt lst] is the string representation of [lst],
    where each element of [lst] has the string representation determined by 
    [string_of_elt]. *)
let string_of_list string_of_elt lst =
  let pp_elts lst =
    let rec loop n acc = function
      | [] -> acc
      | [ h ] -> acc ^ string_of_elt h
      | h1 :: (h2 :: t as t') ->
          if n = 100 then acc ^ "..." (* stop printing long list *)
          else loop (n + 1) (acc ^ string_of_elt h1 ^ ", ") t'
    in
    loop 0 "" lst
  in
  "{| " ^ pp_elts lst ^ " |}"
