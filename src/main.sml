structure Main =
struct

  val pl = SimplePlayer.play

  val _ = Hanabi.newGame [pl,pl,pl]

  val _ = Hanabi.newGamesAllPlayers 200 pl

end
