structure Hanabi :> HANABI =
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
  datatype player = Me | Other of int
  datatype play = Discarded of card
                | Played of card
                | HintedSuit of player * suit * card list
                | HintedRank of player * rank * card list
  datatype turns = Deck of int | Turns of int (* deck size or turns remaining *)

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

  (* Full game state. #hands contains all hands; first hand plays next. *)
  type fstate = {hints : int,
                 fuses : int,
                 hands : (card * info list) list list,
                 log : play list,
                 turns : turns,
                 inDeck : card list,
                 inPlay : rank SD.dict,
                 inDiscard : rank list SD.dict}

  (* Player-visible state. #hands contains all other players' cards; #clues
   * contains current player's clues; #log returns list of last n plays. *)
  type state = {hints : int,
                fuses : int,
                clues : info list list,
                hands : (card * info list) list list,
                log : int -> (player * play) list,
                turns : turns,
                inPlay : rank SD.dict,
                inDiscard : rank list SD.dict}

  (* Boilerplate, printing, miscellaneous {{{ *)
  fun withHints (s : fstate) hints' : fstate =
    {hints = hints', fuses = #fuses s, hands = #hands s, log = #log s, turns =
    #turns s, inDeck = #inDeck s, inPlay = #inPlay s, inDiscard = #inDiscard s}
  fun withFuses (s : fstate) fuses' : fstate =
    {hints = #hints s, fuses = fuses', hands = #hands s, log = #log s, turns =
    #turns s, inDeck = #inDeck s, inPlay = #inPlay s, inDiscard = #inDiscard s}
  fun withHands (s : fstate) hands' : fstate =
    {hints = #hints s, fuses = #fuses s, hands = hands', log = #log s, turns =
    #turns s, inDeck = #inDeck s, inPlay = #inPlay s, inDiscard = #inDiscard s}
  fun withLog (s : fstate) log' : fstate =
    {hints = #hints s, fuses = #fuses s, hands = #hands s, log = log', turns =
    #turns s, inDeck = #inDeck s, inPlay = #inPlay s, inDiscard = #inDiscard s}
  fun withTurns (s : fstate) turns' : fstate =
    {hints = #hints s, fuses = #fuses s, hands = #hands s, log = #log s, turns =
    turns', inDeck = #inDeck s, inPlay = #inPlay s, inDiscard = #inDiscard s}
  fun withInDeck (s : fstate) inDeck' : fstate =
    {hints = #hints s, fuses = #fuses s, hands = #hands s, log = #log s, turns =
    #turns s, inDeck = inDeck', inPlay = #inPlay s, inDiscard = #inDiscard s}
  fun withInPlay (s : fstate) inPlay' : fstate =
    {hints = #hints s, fuses = #fuses s, hands = #hands s, log = #log s, turns =
    #turns s, inDeck = #inDeck s, inPlay = inPlay', inDiscard = #inDiscard s}
  fun withInDiscard (s : fstate) inDiscard' : fstate =
    {hints = #hints s, fuses = #fuses s, hands = #hands s, log = #log s, turns =
    #turns s, inDeck = #inDeck s, inPlay = #inPlay s, inDiscard = inDiscard'}

  fun withCurHand (s : fstate) hand : fstate =
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

  fun actionToString (a : action) =
    case a of
         Discard i => "Discard " ^ Int.toString i
       | Play i => "Play " ^ Int.toString i
       | HintSuit (i,su) => "Hint " ^ suitToString su ^ " to p" ^ Int.toString i
       | HintRank (i,r) => "Hint " ^ Int.toString r ^ " to p" ^ Int.toString i

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

  fun newGameState (numPlayers : int) : fstate =
  let
    val size = if numPlayers = 2 orelse numPlayers = 3 then 5 else 4
    fun makeHands (n : int, xs : 'a list) : 'a list list * 'a list =
      if n = 0 then ([],xs) else
        let
          val (hand,rest) = splitAt (xs, size)
          val (hands,deck) = makeHands (n-1, rest)
        in
          (hand::hands, deck)
        end
    val (hands,deck) = makeHands (numPlayers, newDeck ())
  in
    {hints = 8,
     fuses = 3,
     hands = map (map (fn x => (x,[]))) hands,
     log = [],
     turns = Deck (60 - numPlayers * size),
     inDeck = deck,
     inPlay = foldl (fn (su,d) => SD.insert d su 0) SD.empty suits,
     inDiscard = foldl (fn (su,d) => SD.insert d su []) SD.empty suits}
  end

  fun score (s : fstate) : int = foldl (op +) 0 (map (SD.lookup (#inPlay s)) suits)

  fun gameOver (s : fstate) : bool =
    (#fuses s = 0) orelse (#turns s = Turns 0) orelse (score s = 30)

  (* If discarding/playing, card index must be in bounds. If hinting, player
   * index must be in bounds, suit cannot be rainbow, and hints must be
   * available. *)
  fun illegalMove (a : action, s : fstate) : bool =
    case a of
         Discard i => (i < 0) orelse (i >= length (hd (#hands s)))
       | Play i => (i < 0) orelse (i >= length (hd (#hands s)))
       | HintSuit (i,su) => (i < 0) orelse (i >= length (#hands s) - 1) orelse
                            (su = Rainbow) orelse (#hints s = 0)
       | HintRank (i,r) => (i < 0) orelse (i >= length (#hands s) - 1) orelse
                           (#hints s = 0)

  (* First player draws a card if possible.
   * Otherwise, start or decrement turn counter. *)
  fun drawCard (s : fstate) : fstate =
    case (#inDeck s,#turns s) of
         ([], Deck _) => withTurns s (Turns (length (#hands s)))
       | ([], Turns n) => withTurns s (Turns (n-1))
       | (c::cs, Deck n) => withTurns (withInDeck
           (withCurHand s ((c,[]) :: hd (#hands s))) cs) (Deck (n-1))
       | _ => raise Fail "Illegal game state."

  (* FIXME maintain the log *)
  (* Current player advances the game state by an (assumed legal) action. *)
  fun enact (a : action, s : fstate) : fstate =
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

  (* Runs a game from state s with a list of players ps.
   * Calls trace after each move, and returns final game state.
   * Requires length (#hands s) = length ps. *)
  fun gameLoop (s : fstate)
               (ps : (state -> action) list)
               (trace : action * fstate -> unit)
               : fstate =
    let
      fun rotate xs = (tl xs) @ [hd xs]
      val act = hd ps
        {hints = #hints s,
         fuses = #fuses s,
         clues = map #2 (hd (#hands s)),
         hands = tl (#hands s),
         log = (fn _ => []), (* FIXME *)
         turns = #turns s,
         inPlay = #inPlay s,
         inDiscard = #inDiscard s}
      val s' = if illegalMove (act,s)
               then raise Fail ("Illegal action: " ^ actionToString act ^ "\n")
               else enact (act,s)
    in
      if (trace (act,s'); gameOver s')
      then s'
      else gameLoop (withHands s' (rotate (#hands s'))) (rotate ps) trace
    end

  fun printState (s : fstate) = (print
    ("Hints: " ^ Int.toString (#hints s) ^ " " ^
     "Fuses: " ^ Int.toString (#fuses s) ^ " " ^
     (case #turns s of
           Deck n => "Deck: " ^ (Int.toString n)
         | Turns n => (Int.toString n) ^ " turns remain.") ^ "\n" ^
     "Play: " ^
     (cardsToString (map (fn su => (su,SD.lookup (#inPlay s) su)) suits)) ^
     " (Score: " ^ Int.toString (score s) ^ ")\n" ^
     "Discard: " ^
     (cardsToString (List.concat (map
       (fn su => map (fn r => (su,r)) (SD.lookup (#inDiscard s) su))
       suits))) ^ "\n" ^
     "Hands:\n" ^ (String.concatWith "\n\n" (map handToString (#hands s))) ^ "\n"))

  (* Play a game of Hanabi between players ps. *)
  fun newGame (ps : (state -> action) list) : int =
  let
    val s = if length ps < 2 orelse length ps > 5
            then raise Fail "Invalid number of players."
            else newGameState (length ps)
    fun trace (a,s) = (print (actionToString a ^ "\n"); printState s; print "\n")
  in
    printState s; print "\n"; score (gameLoop s ps trace)
  end

end
