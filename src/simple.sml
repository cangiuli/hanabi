structure SimplePlayer :> PLAYER =
struct

  open Hanabi

  val ranks = [1,1,1,2,2,3,3,4,4,5]
  val suits = [White,Yellow,Green,Blue,Red,Rainbow]
  val suits' = [White,Yellow,Green,Blue,Red]

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

  (* Drawn cards are added to the front of the hand. *)
  fun oldestUnclued (xs : ('a * info list) list) : 'a option =
    Option.map #1 (Util.revFind (fn (_,is) => not (isClued is)) xs)

  (* Returns index of oldest unclued card in our own hand. *)
  fun ourOldestUnclued (xs : info list list) : int option =
    Util.revFindIndex (not o isClued) xs

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

  (* Gives the clued rank of a card, if it exists. *)
  fun cluedRank (is : info list) : int option =
    case List.find (fn i => case i of IsRank _ => true | _ => false) is of
         SOME (IsRank r) => SOME r
       | _ => NONE

  (* Gives the list of negative rank information of a card. *)
  fun cluedNotRank (is : info list) : int list =
    List.mapPartial (fn i => case i of NotRank r => SOME r | _ => NONE) is

  (* Gives the list of negative suit information of a card. *)
  fun cluedNotSuit (is : info list) : suit list =
    List.mapPartial (fn i => case i of NotSuit su => SOME su | _ => NONE) is

  (* Gives the clued suit or Rainbow if multiple suits clued. *)
  fun cluedSuit (is : info list) : suit option =
    case List.mapPartial (fn i => case i of IsSuit su => SOME su | _ => NONE) is of
         [] => NONE
       | x::xs => if List.exists (fn su => su <> x) xs then SOME Rainbow else SOME x

  (* Returns the list of possible suits a card can have. *)
  fun possibleSuits (is : info list) : suit list =
    case cluedSuit is of
         SOME Rainbow => [Rainbow]
       | SOME su => if null (cluedNotSuit is) then [su, Rainbow] else [su]
       | NONE =>
           let
             val sus = cluedNotSuit is
           in
             (List.filter (not o Util.elem sus) suits') @
             (if null sus then [Rainbow] else [])
           end

  (* Returns the highest possible rank of a card. *)
  fun highestPossibleRank (is : info list) : int =
    case cluedRank is of
         SOME i => i
       | NONE => Option.valOf (List.find (fn i => not (Util.elem is (NotRank i)))
                                         [5,4,3,2,1])

  (* From the given clues, is this card useless? *)
  fun isUseless' (s : state) (is : info list) : bool =
  let
    val r = highestPossibleRank is
  in
    List.all (fn su => r <= SD.lookup (#inPlay s) su) (possibleSuits is)
  end

  fun ourOldestUseless (s : state) (xs : info list list) : int option =
    Util.revFindIndex (isUseless' s) xs

  (* Very simple strategy:
   *
   * A number clue including the oldest unclued card is a save clue.
   * Other clues are play clues on the newest clued card.
   *
   * Look at previous action. If it was a play clue for us, play it.
   * Otherwise, look at next player's hand.
   *   If a play clue can be given, give it.
   *   Otherwise, if the oldest unclued card is vital, give a save clue.
   *   Otherwise, discard oldest card known to be already played.
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

  (* Discard useless card in our hand (if possible). *)
  fun discardUseless (s : state) : action option =
    if #hints s = 8
    then NONE
    else Option.map Discard (ourOldestUseless s (#clues s))

  (* Discard oldest unclued card in our hand (if possible). *)
  fun discardOldestUnclued (s : state) : action option =
    if #hints s = 8
    then NONE
    else Option.map Discard (ourOldestUnclued (#clues s))

  val otherwise = Util.otherwise
  infix 4 otherwise

  fun play s =
    receivedPlayHint s otherwise (fn () =>
    givePlayHint s otherwise (fn () =>
    giveSaveHint s otherwise (fn () =>
    discardUseless s otherwise (fn () =>
    discardOldestUnclued s otherwise (fn () =>
    Play (MTRand.randInt (length (#clues s))))))))

end
