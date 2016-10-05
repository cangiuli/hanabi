(* Functions useful in all Hanabi player implementations. *)

structure UtilHanabi =
struct

  open Hanabi

  val ranks = [1,1,1,2,2,3,3,4,4,5]
  val suits = [White,Yellow,Green,Blue,Red,Rainbow]
  val suits' = [White,Yellow,Green,Blue,Red]

  fun allHints (i : int) : action list =
    map (fn su => HintSuit (i,su)) suits' @
    map (fn r => HintRank (i,r)) ranks

  fun numOfRank r = if r = 1 then 3 else if r = 5 then 1 else 2

  fun score (s : state) : int = foldl (op +) 0 (map (SD.lookup (#inPlay s)) suits)

  fun players (s : state) : int = 1 + length (#hands s)

  fun lastRound (s : state) : (player * play) list = #log s (players s)

  fun isPlayable (s : state) ((su,r) : card) = r = (SD.lookup (#inPlay s) su) + 1

  fun isUseless (s : state) ((su,r) : card) = r <= SD.lookup (#inPlay s) su

  fun isVital (s : state) ((su,r) : card) =
    numOfRank r - 1 = Util.count (fn r' => r = r') (SD.lookup (#inDiscard s) su)

  fun isClued (is : info list) = List.exists
    (fn i => case i of IsSuit _ => true | IsRank _ => true | _ => false) is

  fun suitToInt su = case su of
                          White => 0
                        | Yellow => 1
                        | Green => 2
                        | Blue => 3
                        | Red => 4
                        | Rainbow => 5

  structure Suit_Ord : ORDERED =
  struct
    type t = suit
    fun eq (su,su') = (suitToInt su = suitToInt su')
    fun compare (su,su') = Int.compare (suitToInt su, suitToInt su')
  end

  (* Sets of suits and sets of ranks. *)
  structure SSet = ListSet (structure Elem = Suit_Ord)
  structure RSet = ListSet (structure Elem = IntOrdered)

end
