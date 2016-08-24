structure Hanabi =
struct

  datatype suit = White | Yellow | Green | Blue | Red | Rainbow
  (* card rank must be in [1,5] *)
  type rank = int
  type card = suit * rank
  datatype action = Discard of int (* card position *)
                  | Play of int (* card position *)
                  | HintSuit of int * suit (* player, suit *)
                  | HintRank of int * rank (* player, rank *)
  datatype info = IsSuit of suit
                | NotSuit of suit
                | IsRank of rank
                | NotRank of rank

  val ranks = [1,1,1,2,2,3,3,4,4,5]
  val suits = [White,Yellow,Green,Blue,Red,Rainbow]

  (* SD :> DICT where key = suit {{{ *)
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

  structure SD = ListDict (structure Key = Suit_Ord)
  (* }}} *)

  (* When a player receives the state, #hands contains all other players'
   * cards with clues, while #clues contains their own clues only. In the
   * gameplay loop, #hands contains all hands (first hand plays next), and
   * #clues is empty. *)
  type state = {hints : int,
                fuses : int,
                clues : info list list,
                hands : (card * info list) list list,
                turnsLeft : int option,
                inDeck : card list,
                inPlay : rank SD.dict,
                inDiscard : rank list SD.dict}

  (* Boilerplate, printing, miscellaneous {{{ *)
  fun withHints (s : state) hints' : state =
    {hints = hints', fuses = #fuses s, clues = #clues s, hands = #hands s,
    turnsLeft = #turnsLeft s, inDeck = #inDeck s, inPlay = #inPlay s, inDiscard =
    #inDiscard s}
  fun withFuses (s : state) fuses' : state =
    {hints = #hints s, fuses = fuses', clues = #clues s, hands = #hands s,
    turnsLeft = #turnsLeft s, inDeck = #inDeck s, inPlay = #inPlay s, inDiscard =
    #inDiscard s}
  fun withClues (s : state) clues' : state =
    {hints = #hints s, fuses = #fuses s, clues = clues', hands = #hands s,
    turnsLeft = #turnsLeft s, inDeck = #inDeck s, inPlay = #inPlay s, inDiscard =
    #inDiscard s}
  fun withHands (s : state) hands' : state =
    {hints = #hints s, fuses = #fuses s, clues = #clues s, hands = hands',
    turnsLeft = #turnsLeft s, inDeck = #inDeck s, inPlay = #inPlay s,
    inDiscard = #inDiscard s}
  fun withTurnsLeft (s : state) turnsLeft' : state =
    {hints = #hints s, fuses = #fuses s, clues = #clues s, hands = #hands s,
    turnsLeft = turnsLeft', inDeck = #inDeck s, inPlay = #inPlay s,
    inDiscard = #inDiscard s}
  fun withInDeck (s : state) inDeck' : state =
    {hints = #hints s, fuses = #fuses s, clues = #clues s, hands = #hands s,
    turnsLeft = #turnsLeft s, inDeck = inDeck', inPlay = #inPlay s, inDiscard =
    #inDiscard s}
  fun withInPlay (s : state) inPlay' : state =
    {hints = #hints s, fuses = #fuses s, clues = #clues s, hands = #hands s,
    turnsLeft = #turnsLeft s, inDeck = #inDeck s, inPlay = inPlay', inDiscard =
    #inDiscard s}
  fun withInDiscard (s : state) inDiscard' : state =
    {hints = #hints s, fuses = #fuses s, clues = #clues s, hands = #hands s,
    turnsLeft = #turnsLeft s, inDeck = #inDeck s, inPlay = #inPlay s, inDiscard =
    inDiscard'}

  fun withCurHand (s : state) hand : state =
    case #hands s of
         [] => raise Empty
       | _::t => withHands s (hand :: t)

  fun suitColor su = case su of
                         White => Ansi.bright_white
                       | Yellow => Ansi.bright_yellow
                       | Green => Ansi.bright_green
                       | Blue => Ansi.bright_blue
                       | Red => Ansi.bright_red
                       | Rainbow => Ansi.dark_gray

  fun cardToString (su,r) = Ansi.colorStr (Int.toString r) (suitColor su)

  fun cardsToString (cs : card list) = String.concatWith " " (map cardToString cs)

  fun suitToString su = case su of
                            White => "white"
                          | Yellow => "yellow"
                          | Green => "green"
                          | Blue => "blue"
                          | Red => "red"
                          | Rainbow => "rainbow"

  fun infoToString i = case i of
                            IsSuit su => suitToString su
                          | NotSuit su => "!" ^ suitToString su
                          | IsRank r => Int.toString r
                          | NotRank r => "!" ^ Int.toString r

  fun handToString (h : (card * info list) list) = String.concatWith "\n"
    (map (fn (c,is) => cardToString c ^ " (" ^
          String.concatWith "," (map infoToString is) ^ ")")
         h)

  fun splitNth (xs : 'a list, n : int) : 'a * 'a list =
    case (xs,n) of
         ([],_) => raise Subscript
       | (h::t,0) => (h,t)
       | (h::t,n) => let val (elt,xs) = splitNth (t,n-1) in (elt, h :: xs) end

  (* splitAt (xs,n) = (List.take (xs,n), List.drop (xs,n)) *)
  fun splitAt (xs : 'a list, n : int) : 'a list * 'a list =
    case (xs,n) of
         (_,0) => ([],xs)
       | ([],_) => raise Subscript
       | (h::t,n) => let val (take,drop) = splitAt (t,n-1) in (h::take, drop) end

  (* }}} *)

  (* shuffle (arr,n) reorders the first n elements of arr
   * using an in-place Fisher-Yates shuffle. *)
  fun shuffle (arr : 'a array, max : int) : unit =
    if max = 0 then
      ()
    else
      let
        val i = MTRand.randInt max
        val ith = Array.sub (arr, i)
        val maxth = Array.sub (arr, max)
      in
        Array.update (arr, i, maxth);
        Array.update (arr, max, ith);
        shuffle (arr, max - 1)
      end

  (* for now, only supporting 6-suit Hanabi *)
  fun newDeck () : card list =
    let
      fun makeSuit (su : suit) = map (fn x => (su,x)) ranks
      val deck = Array.fromList
        (List.concat (map makeSuit suits))
    in
      shuffle (deck, 59); Array.foldr (op ::) [] deck
    end

  fun newGameState (numPlayers : int) : state =
  let
    val size = if numPlayers = 2 orelse numPlayers = 3 then 5 else 4
    (* FIXME *)
    fun makeHands (n : int, xs : 'a list) : 'a list list =
      case n of
           0 => []
         | _ => List.take (xs,size) :: makeHands (n-1, List.drop (xs,size))
    val deck = newDeck ()
  in
    {hints = 8,
     fuses = 3,
     clues = [],
     hands = map (map (fn x => (x,[]))) (makeHands (numPlayers, deck)),
     turnsLeft = NONE,
     inDeck = List.drop (deck, numPlayers * size),
     inPlay = foldl (fn (su,d) => SD.insert d su 0) SD.empty suits,
     inDiscard = foldl (fn (su,d) => SD.insert d su []) SD.empty suits}
  end

  fun score (s : state) : int = foldl (op +) 0 (map (SD.lookup (#inPlay s)) suits)

  fun gameOver (s : state) : bool =
    (#fuses s = 0) orelse (#turnsLeft s = SOME 0) orelse (score s = 30)

  (* If discarding/playing, card index must be in bounds. If hinting, player
   * index must be in bounds, suit cannot be rainbow, and hints must be
   * available. *)
  fun illegalMove (a : action, s : state) : bool =
    case a of
         Discard i => (i < 0) orelse (i >= length (hd (#hands s)))
       | Play i => (i < 0) orelse (i >= length (hd (#hands s)))
       | HintSuit (i,su) => (i < 0) orelse (i >= length (#hands s) - 1) orelse
                            (su = Rainbow) orelse (#hints s = 0)
       | HintRank (i,r) => (i < 0) orelse (i >= length (#hands s) - 1) orelse
                           (#hints s = 0)

  (* First player draws a card if possible.
   * Otherwise, start or decrement turn counter. *)
  fun drawCard (s : state) : state =
    case (#inDeck s,#turnsLeft s) of
         ([], NONE) => withTurnsLeft s (SOME (length (#hands s)))
       | ([], SOME n) => withTurnsLeft s (SOME (n-1))
       | (c::cs, _) => withInDeck (withCurHand s ((c,[]) :: hd (#hands s))) cs

  (* Current player advances the game state by an (assumed legal) action. *)
  fun enact (a : action, s : state) : state =
    case a of
         Discard i =>
           let
             val (((su,r),_),cs) = splitNth (hd (#hands s), i)
           in
             withInDiscard
               (drawCard (withCurHand s cs))
               (SD.insert (#inDiscard s) su (r :: SD.lookup (#inDiscard s) su))
           end
       | Play i =>
           let
             val (((su,r),_),cs) = splitNth (hd (#hands s), i)
           in
             if (SD.lookup (#inPlay s) su) + 1 = r
             then withInPlay
                    (drawCard (withCurHand s cs))
                    (SD.insert (#inPlay s) su r)
             else withInDiscard
                    (drawCard (withCurHand (withFuses s (#fuses s - 1)) cs))
                    (SD.insert (#inDiscard s) su (r::SD.lookup (#inDiscard s) su))
           end
       | HintSuit (i,su) =>
           let
             val (prev,ith::next) = splitAt (#hands s, i+1)
             fun info (su',r) =
               if su = su' orelse su' = Rainbow then IsSuit su else NotSuit su
           in
             withHands
               (withHints s (#hints s - 1))
               (prev @ [map (fn (c,is) => (c, info c :: is)) ith] @ next)
           end
       | HintRank (i,r) =>
           let
             val (prev,ith::next) = splitAt (#hands s, i+1)
             fun info (su,r') = if r = r' then IsRank r else NotRank r
           in
             withHands
               (withHints s (#hints s - 1))
               (prev @ [map (fn (c,is) => (c, info c :: is)) ith] @ next)
           end

  (* TODO. call illegalMove and enact; hand management stuff. Check for
   * end-of-game after enacting turn. *)
  (* TODO print action as well *)
  fun gameLoop (s : state)
               (ps : (state -> action) list)
               (trace : state -> unit)
               : state =
    let
      fun rotate xs = (tl xs) @ [hd xs]
      val hand::rest = #hands s
      val act = hd ps (withHands (withClues (withInDeck s []) (map #2 hand)) rest)
      val _ = if illegalMove (act,s) then raise Fail "illegal action" else ()
      val s' = enact (act,s)
      val _ = trace s'
    in
      if gameOver s'
      then s'
      else gameLoop (withHands s' (rotate (#hands s'))) (rotate ps) trace
    end

  fun printState (s : state) = (print
    ("Hints: " ^ Int.toString (#hints s) ^ " " ^
     "Fuses: " ^ Int.toString (#fuses s) ^ " " ^
     "Deck: " ^ Int.toString (length (#inDeck s)) ^ "\n" ^
     (* "(Player has " ^ Int.toString (length (#clues s)) ^ " clues)\n" ^ *)
     "Play: " ^
     (cardsToString (map (fn su => (su,SD.lookup (#inPlay s) su)) suits)) ^
     " (Score: " ^ Int.toString (score s) ^ ")\n" ^
     "Discard: " ^
     (cardsToString (List.concat (map
       (fn su => map (fn r => (su,r)) (SD.lookup (#inDiscard s) su))
       suits))) ^ "\n" ^
     "Hands:\n" ^ (String.concatWith "\n\n" (map handToString (#hands s))) ^ "\n" ^
     (case #turnsLeft s of
           NONE => ""
         | SOME n => (Int.toString n) ^ " turns remain.\n")))

  (* TODO *)
  fun newGame () =
  let
    val newState = newGameState 2
  in
    (*
    gameLoop
      newState
      [fn x => HintRank (0,1), fn x => HintSuit (0,Red)] printState
    *)
    printState (enact ((HintRank (0,1)),newState))
  end

end
