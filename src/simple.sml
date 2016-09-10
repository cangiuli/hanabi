structure SimplePlayer :> PLAYER =
struct

  open Hanabi

  (* FIXME move general-purpose Hanabi functions elsewhere *)

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
    numOfRank r - 1 = Util.count (fn r' => r = r') (SD.lookup (#inDiscard s) su)

  fun isClued (is : info list) = List.exists
    (fn i => case i of IsSuit _ => true | IsRank _ => true | _ => false) is

  (* Drawn cards are added to the front of the hand. *)
  fun oldestUnclued (xs : ('a * info list) list) : 'a option =
    Option.map #1 (Util.revFind (fn (_,is) => not (isClued is)) xs)

  (* Returns index of oldest unclued card in our own hand. *)
  fun ourOldestUnclued (xs : info list list) : int option =
    oldestUnclued (ListPair.zip (List.tabulate (5,fn n => n), xs))

  (* Returns index of newest just-clued card in our own hand. *)
  fun ourNewestJustClued (s : state) : int option =
    Util.findIndex (fn is => case is of
                                  (IsSuit _)::_ => true
                                | (IsRank _)::_ => true
                                | _ => false)
                   (#clues s)

  (* Returns the newest card matching the given hint. *)
  fun newestMatching (s : state) (a : action) : card option =
    case a of
         HintSuit (i,su') =>
           List.find (fn (su,r) => su = Rainbow orelse su = su')
             (map #1 (List.nth (#hands s,i)))
       | HintRank (i,r') =>
           List.find (fn (su,r) => r = r') (map #1 (List.nth (#hands s,i)))
       | _ => NONE

  (* Missing features:
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
   * Otherwise, look at next player's hand.
   *   If a play clue can be given, give it.
   *   Otherwise, if the oldest unclued card is vital, give a save clue.
   *   Otherwise, discard oldest unclued card (or if impossible, play randomly).
   *)

  (* Is this action a number clue to me which applies to the oldest card
   * about which we previously had no positive information? *)
  fun isSaveClue (s : state) (_,HintedRank (Me,r,[])) =
    (case ourOldestUnclued (map tl (#clues s)) of
          NONE => false
        | SOME i => (case hd (List.nth (#clues s,i)) of
                          IsRank _ => true
                        | _ => false))
    | isSaveClue s _ = false

  (* Is the most recent action a play clue to me (and if so, for which card)? *)
  fun receivedPlayHint (s : state) : action option =
    case #log s 1 of
         (pl,HintedSuit (Me,su,[]))::_ => Option.map Play (ourNewestJustClued s)
       | (pl,HintedRank (Me,r,[]))::_ => if isSaveClue s (pl,HintedRank (Me,r,[]))
                                         then NONE
                                         else Option.map Play (ourNewestJustClued s)
       | _ => NONE

  (* Does the next player have a hintable, playable card (and if so, how)? *)
  fun givePlayHint (s : state) : action option =
    if #hints s = 0
    then NONE
    else List.find (fn a => Util.maybe false (isPlayable s) (newestMatching s a))
                   (map (fn su => HintSuit (0,su)) [White,Yellow,Green,Blue,Red] @
                    map (fn r => HintRank (0,r)) [1,2,3,4,5])

  (* Is the next player's oldest unclued card vital (if so, save it)? *)
  fun giveSaveHint (s : state) : action option =
    case oldestUnclued (hd (#hands s)) of
         SOME (su,r) => if isVital s (su,r) andalso #hints s > 0
                        then SOME (HintRank (0,r))
                        else NONE
       | NONE => NONE

  (* TODO instead, oldest non-useless card *)
  (* Discard oldest unclued card in our hand (if possible). *)
  fun discardOldestUnclued (s : state) : action option =
    if #hints s = 8
    then NONE
    else Option.map Discard (ourOldestUnclued (#clues s))

  val otherwise = Util.otherwise
  infix 4 otherwise

  (* FIXME - turns mechanism is broken I think? *)

  fun play s =
    receivedPlayHint s otherwise (fn () =>
    givePlayHint s otherwise (fn () =>
    giveSaveHint s otherwise (fn () =>
    discardOldestUnclued s otherwise (fn () =>
    Play (MTRand.randInt (length (#clues s)))))))

end
