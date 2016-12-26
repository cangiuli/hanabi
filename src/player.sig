signature PLAYER =
sig

  (* the integer is the number of players in the game *)
  val play : int -> Hanabi.state -> Hanabi.action

end
