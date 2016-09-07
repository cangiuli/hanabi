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

  (* Applies f to each element from left to right, until f x is true. Returns
   * SOME i if such an x exists at index i, otherwise it returns NONE. *)
  fun findIndex (f : 'a -> bool) (xs : 'a list) : int option =
  let
    fun loop xs i =
      case xs of
           [] => NONE
         | x::xs' => if f x then SOME i else loop xs' (i+1)
  in
    loop xs 0
  end

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

  (* Returns index of oldest unclued card in our own hand. *)
  fun ourOldestUnclued (xs : info list list) : int option =
    oldestUnclued (ListPair.zip (List.tabulate (5,fn n => n), xs))

  (* Returns index of newest just-clued card in our own hand. *)
  fun ourNewestJustClued (s : state) : int option =
    findIndex (fn is => case is of
                             (IsSuit _)::_ => true
                           | (IsRank _)::_ => true
                           | _ => false)
              (#clues s)

  (* TODO Missing features:
   * - finesse, reverse finesse, bluff
   * - endgame awareness
   * - to be a save clue, must be possibly both vital and unplayable
   * - save information across invocations
   *)

  (* Very simple strategy:
   *
   * A number clue including the oldest unclued card is a save clue.
   * Other clues are play clues on the newest clued card.
   *
   * Look at previous action. If it was a play clue for us, play it.
   * TODO - if it was a save clue for us, note its possible values
   *
   * Otherwise, look at next player's hand.
   *   If a play clue can be given, give it.
   *   Otherwise, if the oldest unclued card is vital, give a save clue.
   *   Otherwise, discard oldest unclued card.
   *)

  (* For p the most recent play, is it a number clue to me which applies to the
   * oldest card about which we previously had no positive information? *)
  fun isSaveClue (s : state) (_,HintedRank (Me,r,[])) =
    (case ourOldestUnclued (map tl (#clues s)) of
          NONE => false
        | SOME i => (case hd (List.nth (#clues s,i)) of
                          IsRank _ => true
                        | _ => false))
    | isSaveClue s _ = false

  (* Is the most recent action a play clue to me (and if so, for which card)? *)
  fun checkForPlayHint (s : state) : action option =
    case #log s 1 of
         (pl,HintedSuit (Me,su,[]))::_ => Option.map Play (ourNewestJustClued s)
       | (pl,HintedRank (Me,r,[]))::_ => if isSaveClue s (pl,HintedRank (Me,r,[]))
                                         then NONE
                                         else Option.map Play (ourNewestJustClued s)
       | _ => NONE

  (* TODO *)
  fun play s =
    case checkForPlayHint s of
         SOME a => a
       | NONE => Play 0

end
