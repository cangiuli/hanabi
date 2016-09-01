structure NaivePlayer :> PLAYER =
struct

  open Hanabi

  (* FIXME move general-purpose functions elsewhere *)

  val ranks = [1,1,1,2,2,3,3,4,4,5]
  val suits = [White,Yellow,Green,Blue,Red,Rainbow]

  fun score (s : state) : int = foldl (op +) 0 (map (SD.lookup (#inPlay s)) suits)

  fun players (s : state) : int = 1 + length (#hands s)

  fun lastRound (s : state) : (player * play) list = #log s (players s)

  (* TODO *)
  fun play s = Play 0

end
