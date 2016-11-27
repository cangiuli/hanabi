structure Main =
struct

  (* val pl = SimplePlayer.play *)
  val pl = AdvancedPlayer.play

  val _ = Hanabi.newGame [pl,pl,pl]

  val _ = Hanabi.newGamesAllPlayers 100 pl

end
