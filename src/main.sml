structure Main =
struct

  val _ = Hanabi.newGame [NaivePlayer.play,NaivePlayer.play,NaivePlayer.play]

end
