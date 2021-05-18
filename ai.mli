(** [command st] is the ai command given after viewing the current game
    state [st]. *)
val command : State.t -> Command.t
