(* Functions useful in all Hanabi player implementations. *)

structure UtilHanabi =
struct

  open Hanabi

  (* {{{ Suit_Ord : ORDERED where t = suit *)
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
  (* }}} *)

  (* Sets of suits and sets of ranks. *)
  structure SSet = ListSet (structure Elem = Suit_Ord)
  structure RSet = ListSet (structure Elem = IntOrdered)

  val suits = foldl (fn (x,s) => SSet.insert s x) SSet.empty
    [White,Yellow,Green,Blue,Red,Rainbow]
  val suits' = SSet.remove suits Rainbow
  val ranks = foldl (fn (x,s) => RSet.insert s x) RSet.empty [1,2,3,4,5]

  fun rainbowAnd su = SSet.insert (SSet.singleton Rainbow) su

  fun allHints (i : int) : action list =
    map (fn su => HintSuit (i,su)) (SSet.toList suits') @
    map (fn r => HintRank (i,r)) [1,2,3,4,5]

  fun numOfRank r = if r = 1 then 3 else if r = 5 then 1 else 2

  fun score (s : state) : int =
    foldl (op +) 0 (map (SD.lookup (#inPlay s)) (SSet.toList suits))

  fun players (s : state) : int = 1 + length (#hands s)

  fun clues (s : state) (pl : player) : info list list =
  case pl of
       Me => #clues s
     | Other i => map #2 (List.nth (#hands s, i))

  fun all_clues (s : state) : info list list list =
    #clues s :: map (map #2) (#hands s)

  fun lastRound (s : state) : (player * play) list = #log s (players s)

  fun isPlayable (s : state) ((su,r) : card) = r = (SD.lookup (#inPlay s) su) + 1

  fun isUseless (s : state) ((su,r) : card) = r <= SD.lookup (#inPlay s) su

  fun isVital (s : state) ((su,r) : card) =
    numOfRank r - 1 = Util.count (fn r' => r = r') (SD.lookup (#inDiscard s) su)

  fun isClued (is : info list) = List.exists
    (fn i => case i of IsSuit _ => true | IsRank _ => true | _ => false) is

  fun playerToInt (pl : player) : int =
    case pl of Me => 0 | Other i => i + 1

  fun intToPlayer (j : int) : player =
  if j = 0 then Me else Other (j-1)

  (* previous turn if the last action involved a play/discard *)
  fun prevTurnDraw (numberofplayers : int) (t : turns) : turns =
    case t of
         Deck n => Deck (n+1)
       | Turns n => if n = numberofplayers then Deck 1 else Turns (n+1)

  (* previous turn if the last action did not involve a play/discard *)
  fun prevTurnNoDraw (numberofplayers : int) (t : turns) : turns =
    case t of
         Deck n => Deck n
       | Turns n => if n = numberofplayers then Deck 1 else Turns (n+1)

  (* If l is a list of information about cards in a players hand, this updates the list after a
   * card has been played/discarded from the hand *)
  fun removeCard (s : state) (l : 'a list) (x : int) (default : 'a) : 'a list =
  let
    val l2 : 'a list = #2 (Util.splitNth (l, x))
  in
    case #turns s of
         Deck _ => default::l2
       | Turns _ => l2
  end

  (* Returns the state of the previous turn when the last action was a.
   * Does not rotate the players. Does not preserve log *)
  fun getPrevState (s : state) ((pl, p) : player * play) : state =
  let
      (* it's not very efficient to use the log of the previous state, better to have computed the
       * log further back already *)
      val n = players s
      val t = prevTurnDraw n (#turns s)
      fun newLog (n : int) = tl (#log s (n+1))
      fun prevClues (j : int) (is : info list) =
        case (pl, t) of
             (Other _, _) => #clues s
           | (Me, Deck _) => Util.insert is (tl (#clues s)) j
           | (Me, Turns _) => Util.insert is (#clues s) j
      fun prevHands (j : int) (x : card * info list) =
        case (pl, t) of
             (Me, _) => #hands s
           | (Other pl', Deck _) => Util.mapAt (fn l => Util.insert x (tl l) j) (#hands s) pl'
           | (Other pl', Turns _) => Util.mapAt (fn l => Util.insert x l j) (#hands s) pl'

      fun forgetHint (pl' : player) : state =
        {hints = #hints s + 1,
         fuses = #fuses s,
         clues = case pl' of
                      Me => map tl (#clues s)
                    | Other _ => #clues s,
         hands = case pl' of
                      Me => #hands s
                    | Other j => Util.mapAt (map (fn (c, is) => (c, tl is))) (#hands s) j,
         log = newLog,
         turns = prevTurnNoDraw n (#turns s),
         turnNumber = #turnNumber s - 1,
         inPlay = #inPlay s,
         inDiscard = #inDiscard s}
  in
    case p of
         Discarded (j,(su,r),is) =>
           {hints = #hints s - 1,
            fuses = #fuses s,
            clues = prevClues j is,
            hands = prevHands j ((su,r),is),
            log = newLog,
            turns = t,
            turnNumber = #turnNumber s - 1,
            inPlay = #inPlay s,
            inDiscard = SD.insert (#inDiscard s) su (tl (SD.lookup (#inDiscard s) su))}

       | Played (j,(su,r),b,is,rh) =>
           {hints = if rh then #hints s - 1 else #hints s,
            fuses = if b then #fuses s else #fuses s + 1,
            clues = prevClues j is,
            hands = prevHands j ((su,r),is),
            log = newLog,
            turns = t,
            turnNumber = #turnNumber s - 1,
            inPlay = if b then SD.insert (#inPlay s) su (SD.lookup (#inPlay s) su - 1)
                          else #inPlay s,
            inDiscard = if b then #inDiscard s
                             else SD.insert (#inDiscard s) su (tl (SD.lookup (#inDiscard s) su))}
       | HintedSuit (pl',_,_) => forgetHint pl'
       | HintedRank (pl',_,_) => forgetHint pl'
  end

  (* Returns the state of the last n turns (in reverse chronological order) assuming that the
     last n actions are given by list l. (The first element is the state of the previous turn.) *)
  fun getPrevStates (s : state) (l : (player * play) list) : state list =
    case l of
         [] => []
       | c::cs => let val s' = getPrevState s c in s'::getPrevStates s' cs end
end
