structure AdvancedPlayer :> PLAYER =
struct

  open UtilHanabi

  (* The memory of a player.
     playable gives the set of playable card indices for each player *)
  type memory = {playable : bool list list,
                 test : int}

  fun initialMemory (num : int) : memory =
  let
      val k = if num = 2 orelse num = 3 then 5 else 4
  in
    {playable = List.tabulate (num, fn _ => List.tabulate (k, fn _ => false)), test = 0}
  end

  fun boolToString (b : bool) = case b of true => "T" | false => "F"
  fun memoryToString (m : memory) =
    String.concatWith "," (map (fn bs => String.concat (map boolToString bs)) (#playable m))

  fun withPlayable (m : memory) playable' : memory =
    {playable = playable', test = #test m}

  fun withTest (m : memory) test' : memory =
    {playable = #playable m, test = test'}

  (* Returns index of oldest unclued card in xs.
   * Drawn cards are added to the front of the hand. *)
  fun oldestUnclued (xs : info list list) : int option =
    Util.revFindIndex (not o isClued) xs

  (* Returns index of newest just-clued card in our own hand. *)
  (* fun ourNewestJustClued (s : state) : int option = *)
  (*   Util.findIndex (fn is => case is of *)
  (*                                 (IsSuit _)::_ => true *)
  (*                               | (IsRank _)::_ => true *)
  (*                               | _ => false) *)
  (*                  (#clues s) *)

  (* Returns index of newest just-clued card in the hand of player pl. *)
  fun newestJustClued (s : state) (pl : player) : int option =
    Util.findIndex (fn is => case is of
                                  (IsSuit _)::_ => true
                                | (IsRank _)::_ => true
                                | _ => false)
                   (clues s pl)

  (* Returns all cards matching the given hint. *)
  fun allMatching (s : state) (a : action) : card list =
    case a of
         HintSuit (i,su') =>
           List.map #1 (List.filter (fn ((su,_),_) => su = Rainbow orelse su = su')
                                    (List.nth (#hands s,i)))
       | HintRank (i,r') =>
           List.map #1 (List.filter (fn ((_,r),_) => r = r') (List.nth (#hands s,i)))
       | _ => []

  (* Returns the indices of all cards matching the given hint. *)
  fun allMatchingIndices (s : state) (a : action) : int list =
    case a of
         HintSuit (i,su') => Util.findIndices (fn ((su,_),_) => su = Rainbow orelse su = su')
                                              (List.nth (#hands s,i))
       | HintRank (i,r') => Util.findIndices (fn ((_,r),_) => r = r') (List.nth (#hands s,i))
       | _ => []

  (* Returns the newest card matching the given hint. *)
  fun newestMatching (s : state) (a : action) : card option =
    case a of
         HintSuit (i,su') =>
           Option.map #1 (List.find (fn ((su,_),_) => su = Rainbow orelse su = su')
                                    (List.nth (#hands s,i)))
       | HintRank (i,r') =>
           (* Enabling the following code will return NONE if the hint can be interpreted
            * as save clue *)
           (* let *)
           (*   val xs = List.nth (#hands s,i) *)
           (*   val c : card option = Option.map #1 (List.find (fn ((_,r),_) => r = r') xs) *)
           (* in *)
           (*   case oldestUnclued (map #2 xs) of *)
           (*        NONE => c *)
           (*      | SOME i => if #2 (#1 (List.nth (xs, i))) = r' then NONE else c *)
           (* end *)
           Option.map #1 (List.find (fn ((_,r),_) => r = r') (List.nth (#hands s,i)))
       | _ => NONE

  (* Returns the (directly) clued rank of a card, if it exists. *)
  fun cluedRank (is : info list) : rank option =
    case List.find (fn i => case i of IsRank _ => true | _ => false) is of
         SOME (IsRank r) => SOME r
       | _ => NONE

  (* Returns the set of possible ranks of a card. *)
  fun possibleRanks (is : info list) : RSet.set =
    case is of
         (IsRank r)::_ => RSet.singleton r
       | (NotRank r)::xs => RSet.remove (possibleRanks xs) r
       | _::xs => possibleRanks xs
       | [] => ranks

  (* Returns the (directly) clued suit, or Rainbow if there is more than one. *)
  fun cluedSuit (is : info list) : suit option =
    case List.mapPartial (fn i => case i of IsSuit su => SOME su | _ => NONE) is of
         [] => NONE
       | x::xs => if List.exists (fn su => su <> x) xs then SOME Rainbow else SOME x

  (* Returns the set of possible suits of a card. *)
  fun possibleSuits (is : info list) : SSet.set =
    case is of
         (IsSuit su)::xs => SSet.intersection (possibleSuits xs) (rainbowAnd su)
       | (NotSuit su)::xs => SSet.difference (possibleSuits xs) (rainbowAnd su)
       | _::xs => possibleSuits xs
       | [] => suits

  exception Unreachable
  fun possibleSuitsAndRanks (is : info list) : SSet.set * RSet.set =
    case is of
         [] => (suits, ranks)
       | (IsRank r)::xs => (possibleSuits xs, RSet.singleton r)
       | i::xs => let val (sus,rs) = possibleSuitsAndRanks xs
                  in case i of
                          IsSuit su => (SSet.intersection sus (rainbowAnd su), rs)
                        | NotSuit su => (SSet.difference sus (rainbowAnd su), rs)
                        | NotRank r => (sus, RSet.remove rs r)
                        | _ => raise Unreachable

                  end


  (* Is this card guaranteed to be useless? *)
  fun isUseless' (s : state) (is : info list) : bool =
  let
    val maxRank = RSet.foldl Int.max 0 (possibleRanks is)
    val allSuits = SSet.toList (possibleSuits is)
  in
    List.all (fn su => maxRank <= SD.lookup (#inPlay s) su) allSuits
  end

  fun ourOldestUseless (s : state) (xs : info list list) : int option =
    Util.revFindIndex (isUseless' s) xs

  (* Returns whether a card is definitely not playable *)
  fun isNotPlayable (s : state) (is : info list) : bool =
  let
    val sus = SSet.toList (possibleSuits is)
    val rs = possibleRanks is
  in
    List.all (fn su => not (RSet.member rs (SD.lookup (#inPlay s) su + 1))) sus
  end

  (* Returns whether a card is definitely playable *)
  fun isDefinitelyPlayable (s : state) (is : info list) : bool =
  let
    val sus = SSet.toList (possibleSuits is)
    val rs = RSet.toList (possibleRanks is)
  in
    length rs = 1 andalso List.all (fn su => hd rs = SD.lookup (#inPlay s) su + 1) sus
  end

  datatype fuzzyBool = True | False | Maybe

  fun isPlayableFuzzy (s : state) (is : info list) : fuzzyBool =
  let
    val (sus', rs') = possibleSuitsAndRanks is
    val sus = SSet.toList sus'
    val rs = RSet.toList rs'
  in
    if length rs = 1 andalso List.all (fn su => hd rs = SD.lookup (#inPlay s) su + 1) sus
    then True
    else if List.all (fn su => not (RSet.member rs' (SD.lookup (#inPlay s) su + 1))) sus
    then False
    else
        Maybe
  end

  fun FuzzyBoolToBool (k : fuzzyBool) (default : bool) : bool =
  case k of
       True => true
     | False => false
     | Maybe => default

  (* check whether a card in your own hand is definitely not vital *)
  fun isNotVital (s : state) (is : info list) : bool =
  let
    val rs = possibleRanks is
    val sus = possibleSuits is
  in
    if RSet.member rs 5 then false else
      List.all (fn su => List.all (fn r => isUseless s (su, r) orelse not (isVital s (su, r)))
                                  (RSet.toList rs))
               (SSet.toList sus)

  end


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
   *   Otherwise, discard oldest unclued card
   *   Otherwise, try to give to the next player which cannot be interpreted as a play hint
   *   Otherwise, discard the least vital card
   *   Otherwise you have 8 clues, and then you clue 1 to the next player.
   *)

  (* Is a rank hint which hints the cards in positions l interpreted as a save hint? *)
  fun isSaveClue (s : state) (pl : player) (l : int list) : bool =
    case oldestUnclued (clues s pl) of
         NONE => false
       | SOME i => Util.elem l i

  (* update the list of playable cards for a single player after a single action *)
  fun updatePlayableAfterAction (s : state) (p : play) (bs : bool list) : bool list =
  case p of
       HintedSuit (pl,su,l) =>
         if null l then bs
         else Util.mapAt (fn _ => true) bs (hd l)
     | HintedRank (pl,r,l) =>
         if null l orelse isSaveClue s pl l then bs
         else Util.mapAt (fn _ => true) bs (hd l)
     | Discarded (j,(su,r),is) => removeCard s bs j false
     | Played (j,(su,r),_,is,_) => removeCard s bs j false

  (* The target of an action. That is the player who did it for plays/discards,
     and the receiver for hints*)
  fun actionTarget (a : player * play) : player =
  case a of
       (pl',HintedSuit (pl,_,_)) => pl
     | (pl',HintedRank (pl,_,_)) => pl
     | (pl, _) => pl

  (* update the list of playable cards in all hands (WIP) *)
  fun updatePlayable (s : state) (m' : bool list list) : bool list list =
  let
    val num = players s
    val cs = #log s num
    val ss = getPrevStates s cs
    (* adds cards which received play clue to list of playable cards *)
    fun updateLoop (((s',(pl,p)), m) : (state * (player * play)) * bool list list)
      : bool list list =
    Util.mapAt (fn bs => updatePlayableAfterAction s' p bs) m (playerToInt (actionTarget (pl, p)))

    (* The knowledge of playable cards, without taking into account that the hinted card might be
       unplayable, and without taking into account that cards other than the first hinted card might
       now be certainly playable/unplayable *)
    val m : bool list list = foldr updateLoop m' (ListPair.zipEq (ss, cs))
    (* add every certainly playable card and remove every certainly nonplayable card *)
    fun finalize_card ((b, is) : bool * info list) : bool =
    FuzzyBoolToBool (isPlayableFuzzy s is) b
  in
    map (map finalize_card o ListPair.zipEq) (ListPair.zipEq (m, allClues s))
  end

  fun allPlayableCards (s : state) : card list =
    map (fn su => (su, SD.lookup (#inPlay s) su + 1)) (SSet.toList suits)

  fun allCluedPlayable (s : state) (m : memory) : card list =
    List.concat (map (fn xs => List.mapPartial (fn (b, c) => case b of
                                                      true => SOME (#1 c)
                                                    | false => NONE)
                                        (ListPair.zipEq xs))
                   (ListPair.zipEq (tl (#playable m), #hands s)))

  fun allUncluedPlayable (s : state) (m : memory) : card list =
    List.filter (fn c => not (Util.elem (allCluedPlayable s m) c)) (allPlayableCards s)

  val uselessCluedValue : int = ~150
  val vitalCluedValue : int = 80
  val playableCluedValue : int = 45
  val canBeSaveValue : int = ~200 (* lower if new chop is vital *)
  (* val otherCluedValue : int = 0 (* this should be split up: usually positive (around 20), *)
  (*                            4s (or too much 3s) early negative *) *)
  (* val uselessClarifiedValue : int = 60 *)
  (* val playableClarifiedValue : int = 50 *)
  (* val otherClarifiedValue : int = 20 *)
  (* val makeCardPlayable : int = 30 *)
  (* other possible penalties:
     marking duplicate cards,
     mark cards which are marked in other players' hands
     mark cards which are possibly marked in my hand *)

  fun clueValue (s : state) (a : action) : int =
  let
    val cs = allMatching s a
    val l = [(Util.count (isUseless s) cs) * uselessCluedValue] @
            [(Util.count (isVital s) cs) * vitalCluedValue] @
            [(Util.count (isPlayable s) cs) * playableCluedValue] @
            (case a of
                  HintSuit (_,_) => []
                | HintRank (i,_) => if isSaveClue s (Other i) (allMatchingIndices s a)
                                    then [canBeSaveValue]
                                    else []
                | _ => [])
  in
    foldr (fn (i, j) => i + j) 0 l
  end

  (*     fun isPlayable (s : state) ((su,r) : card) = r = (SD.lookup (#inPlay s) su) + 1 *)

  (* fun isUseless (s : state) ((su,r) : card) = r <= SD.lookup (#inPlay s) su *)

  (* fun isVital (s : state) ((su,r) : card) = *)
  (*   numOfRank r - 1 = Util.count (fn r' => r = r') (SD.lookup (#inDiscard s) su) *)


  (* Try to give a play hint to player (Other i).
   * Don't give a clue if someone already has that card marked as playable.
   * TODO: also don't give it if it's marked in anyone's hand (but not marked as playable) *)
  fun givePlayHint (s : state) (m : memory) (i : int) : action option =
  if #hints s = 0 then NONE
  else let
    val wantToClue : card list =
      List.mapPartial (fn (c, _) => if Util.elem (allUncluedPlayable s m) c then SOME c else NONE)
                      (List.nth (#hands s, i))
    val workingClues : action list =
      List.filter (fn a => case newestMatching s a of
                                NONE => false
                              | SOME c => Util.elem wantToClue c)
                  (allHints i)
  in
    if null workingClues then NONE
    else SOME (Util.findMax (clueValue s) workingClues)
  end

  fun giveLaterPlayHint (s : state) (m : memory) : action option =
  if #hints s < 2 then NONE
 else Util.findPartial (givePlayHint s m) (List.tabulate (length (#hands s)-1, fn i => i+1))

  (* Is the next player's oldest unclued card vital (if so, save it)? *)
  fun giveSaveHint (s : state) : action option =
    case oldestUnclued (clues s (Other 0)) of
         SOME i => let val ((su,r),_) = List.nth (hd (#hands s), i)
                   in
                        if isVital s (su,r) andalso #hints s > 0
                        then SOME (HintRank (0,r))
                        else NONE
                   end
       | NONE => NONE

  (* Does player (Other pl) need a save hint? *)
  fun needSaveHint (s : state) (pl : int) : bool =
    case oldestUnclued (clues s (Other pl)) of
         SOME i => isVital s (#1 (List.nth (List.nth (#hands s, pl), i)))
       | NONE => false

  (* Discard useless card in our hand (if possible). *)
  fun discardUseless (s : state) : action option =
    if #hints s = 8
    then NONE
    else Option.map Discard (ourOldestUseless s (#clues s))

  (* Discard oldest unclued card in our hand (if possible). *)
  fun discardOldestUnclued (s : state) : action option =
    if #hints s = 8
    then NONE
    else Option.map Discard (oldestUnclued (#clues s))

  (* Try to give a hint pointing at 0 cards.
   * Currently it will only try to give rank hints to the next player, but this can be extended *)
  fun giveBlankHint (s : state) : action option =
    if #hints s = 0
    then NONE
    else Option.map (fn i => HintRank (0, i))
                    (List.find (fn i => not (List.exists (fn x => #2 (#1 x) = i) (hd (#hands s))))
                               [5,4,3,2,1])

  (* Give a hint which cannot be interpreted as a play hint
   * This will hint 5 if that hint cannot be interpreted as a play hint;
   * Otherwise it will hint 1 if all 1s have been played;
   * Otherwise it will try to give a blank hint *)
  fun wasteHint (s : state) : action option =
    if #hints s = 0
    then NONE
    else if List.all (fn su => SD.lookup (#inPlay s) su <> 4) (SSet.toList suits)
    then SOME (HintRank (0, 5))
    else if List.all (fn su => SD.lookup (#inPlay s) su > 0) (SSet.toList suits)
    then SOME (HintRank (0, 1))
    else giveBlankHint s

  val otherwise = Util.otherwise
  infix 4 otherwise

  (* Try to find the least harmful discard action.
   * It first tries to discard the oldest card known to be not vital (the oldest);
   * Then it will try to discard a card not marked with number (the newest);
   * (TO DO: discard card which was never saved when it was on the chop)
   * Otherwise it will discard the card clued with the highest rank (the oldest).
   * This function assumes that discardOldestUnclued returned NONE, and only returns NONE if there
     are 8 hints available *)
  fun discardAny (s : state) : action option =
    if #hints s = 8 then NONE else
    SOME (Option.map Discard (Util.revFindIndex (isNotVital s) (#clues s)) otherwise (fn () =>
          Option.map Discard (Util.findIndex (not o isSome o cluedRank) (#clues s))
                     otherwise (fn () =>
          Discard (Util.revFindMaxIndex (valOf o cluedRank) (#clues s)))))

  (* The order of actions should become:
   * - (respond to finesse)
   * - if chop of next player is vital or playable:
   *   = give play clue on a card which is not clued in your hand to next player
   *   = give a save clue
   * - play a playable card
   * - give any play clue *)
  fun play (num : int) : state -> action =
  let
    val m : memory ref = ref (initialMemory num)
  in
    fn s => (
    m := withPlayable (!m) (updatePlayable s (#playable (!m)));
    (* if I have a playable card, give a save hint, otherwise play *)
    Option.map (fn i => giveSaveHint s otherwise (fn () => Play i))
               (Util.findIndex (fn b => b) (hd (#playable (!m)))) otherwise (fn () =>
    givePlayHint s (!m) 0 otherwise (fn () =>
    giveSaveHint s otherwise (fn () =>
    giveLaterPlayHint s (!m) otherwise (fn () =>
    discardUseless s otherwise (fn () =>
    discardOldestUnclued s otherwise (fn () =>
    wasteHint s otherwise (fn () =>
    discardAny s otherwise (fn () =>
    HintRank (0, 1))))))))))
  end

end
