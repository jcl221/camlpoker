type command =
| Bet of int
| Check
| Raise of int
| Call
| Fold
| Invalid

val parse : string -> command