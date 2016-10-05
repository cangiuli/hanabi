structure Main =
struct

  val pl = SimplePlayer.play

  val _ = Hanabi.newGame [pl,pl,pl]

  val scores = Hanabi.newGames 20 [pl,pl,pl]
  val _ = print (String.concatWith "," (map Int.toString scores) ^ "\n")

end
