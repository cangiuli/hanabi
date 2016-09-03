structure SimplePlayer :> PLAYER =
struct

  open Hanabi

  (* FIXME move general-purpose functions elsewhere *)

  (* Returns the number of elements of xs satisfying f. *)
  fun count (f : 'a -> bool) (xs : 'a list) : int =
    foldl (fn (x,n) => if f x then n+1 else n) 0 xs

  (* Applies f to each element from right to left, until f x is true.
   * Returns SOME x if such an x exists, otherwise it returns NONE. *)
  fun revFind (f : 'a -> bool) (xs : 'a list) : 'a option =
    if null xs then NONE else
      case revFind f (tl xs) of
           NONE => if f (hd xs) then SOME (hd xs) else NONE
         | SOME y => SOME y

  val ranks = [1,1,1,2,2,3,3,4,4,5]
  val suits = [White,Yellow,Green,Blue,Red,Rainbow]

  fun numOfRank r = if r = 1 then 3 else if r = 5 then 1 else 2

  fun score (s : state) : int = foldl (op +) 0 (map (SD.lookup (#inPlay s)) suits)

  fun players (s : state) : int = 1 + length (#hands s)

  fun lastRound (s : state) : (player * play) list = #log s (players s)

  fun isPlayable (s : state) ((su,r) : card) = r = (SD.lookup (#inPlay s) su) + 1

  (* TODO also check for unreachable cards *)
  fun isUseless (s : state) ((su,r) : card) = r <= SD.lookup (#inPlay s) su

  fun isVital (s : state) ((su,r) : card) =
    numOfRank r - 1 = count (fn r' => r = r') (SD.lookup (#inDiscard s) su)

  fun isClued (is : info list) = List.exists
    (fn i => case i of IsSuit _ => true | IsRank _ => true | _ => false) is

  (* Drawn cards are added to the front of the hand. *)
  fun oldestUnclued (xs : ('a * info list) list) : 'a option =
    Option.map #1 (revFind (fn (_,is) => not (isClued is)) xs)

  (* TODO Missing features:
   * - finesse, reverse finesse, bluff
   * - endgame awareness
   * - to be a save clue, must be possibly both vital and unplayable
   * - save information across invocations
   *)

  (* code fragments:
     oldestUnclued (ListPair.zip (List.tabulate (5,fn n => n), #clues s))
   *)

  (* TODO *)
  (* Simple strategy:
   *
   * A number clue including the oldest unclued card is a save clue.
   * Other clues are play clues on the newest clued card.
   *
   * Look at last round of clues.
   *   If we received a save clue, note it.
   *   If we received a play clue, play it.
   *
   * Otherwise, look at next player's hand.
   *   If a play clue can be given, give it.
   *   Otherwise, if the oldest unclued card is vital, give a save clue.
   *   Otherwise, discard oldest unclued card.
   *)
  fun play s = Play 0

end
