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

  (* SD :> DICT where key = suit {{{ *)
  fun suitToInt s = case s of
                         White => 0
                       | Yellow => 1
                       | Green => 2
                       | Blue => 3
                       | Red => 4
                       | Rainbow => 5

  structure Suit_Ord : ORDERED =
  struct
    type t = suit
    fun eq (s,s') = (suitToInt s = suitToInt s')
    fun compare (s,s') = Int.compare (suitToInt s, suitToInt s')
  end

  structure SD = ListDict (structure Key = Suit_Ord)
  (* }}} *)

  type state = {hints : int,
                fuses : int,
                clues : info list list,
                hands : (card * info list) list list,
                turnsLeft : int option,
                inDeck : card list,
                inPlay : rank SD.dict,
                inDiscard : rank list SD.dict}

  (* Boilerplate and printing {{{ *)
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
  fun withOnTurns (s : state) turnsLeft' : state =
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

  fun suitColor s = case s of
                         White => Ansi.bright_white
                       | Yellow => Ansi.bright_yellow
                       | Green => Ansi.bright_green
                       | Blue => Ansi.bright_blue
                       | Red => Ansi.bright_red
                       | Rainbow => Ansi.dark_gray

  fun cardToString (s,n) = Ansi.colorStr (Int.toString n) (suitColor s)

  fun cardsToString (cs : card list) = String.concatWith " " (map cardToString cs)

  fun suitToString s = case s of
                            White => "white"
                          | Yellow => "yellow"
                          | Green => "green"
                          | Blue => "blue"
                          | Red => "red"
                          | Rainbow => "rainbow"

  fun infoToString i = case i of
                            IsSuit s => suitToString s
                          | NotSuit s => "not " ^ suitToString s
                          | IsRank r => Int.toString r
                          | NotRank r => "not " ^ Int.toString r


  fun handToString (h : (card * info list) list) = String.concatWith "\n"
    (map (fn (c,is) => cardToString c ^ " (" ^
          String.concatWith " " (map infoToString is) ^ ")")
         h)

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

  val ranks = [1,1,1,2,2,3,3,4,4,5]
  val suits = [White,Yellow,Green,Blue,Red,Rainbow]

  (* for now, only supporting 6-suit Hanabi *)
  fun newDeck () : card list =
    let
      fun makeSuit (s : suit) = map (fn x => (s,x)) ranks
      val deck = Array.fromList
        (List.concat (map makeSuit suits))
    in
      shuffle (deck, 59); Array.foldr (op ::) [] deck
    end

  (* for now, only supporting 2-player Hanabi *)
  fun newGameState () : state =
  let
    (* FIXME *)
    val deck = newDeck ()
    val firstHand = map (fn x => (x,[])) (List.take (deck,5))
    val secondHand = map (fn x => (x,[])) (List.take (List.drop (deck,5), 5))
    val deck' = List.drop (deck,10)
  in
    {hints = 8,
     fuses = 3,
     clues = [],
     hands = [firstHand, secondHand],
     turnsLeft = NONE,
     inDeck = deck',
     inPlay = foldl (fn (s,d) => SD.insert d s 0) SD.empty suits,
     inDiscard = foldl (fn (s,d) => SD.insert d s []) SD.empty suits}
  end

  fun score (s : state) : int = foldl (op +) 0 (map (SD.lookup (#inPlay s)) suits)

  fun isGameOver (s : state) : bool =
    (#fuses s = 0) orelse (#turnsLeft s = SOME 0) orelse (score s = 30)

  (* FIXME also check Discard/Play for index, HintSuit/HintRank for player #,
   * hintSuit for rainbow *)
  fun illegalMove (a : action, s : state) =
    (case a of HintSuit _ => true | HintRank _ => true | _ => false) andalso
    (#hints s = 0)

  (* TODO *)
  fun enact (a : action, s : state) : state = s

  (* TODO *)
  fun gameLoop (s : state) : state = s

  fun printState (s : state) = (print
    ("Hints: " ^ Int.toString (#hints s) ^ " " ^
     "Fuses: " ^ Int.toString (#fuses s) ^ " " ^
     "Deck: " ^ Int.toString (length (#inDeck s)) ^ "\n" ^
     (* "(Player has " ^ Int.toString (length (#clues s)) ^ " clues)\n" ^ *)
     "Play: " ^
     (cardsToString (map (fn suit => (suit,SD.lookup (#inPlay s) suit)) suits)) ^
     " (Score: " ^ Int.toString (score s) ^ ")\n" ^
     "Discard: " ^
     (cardsToString (List.concat (map
       (fn suit => map (fn r => (suit,r)) (SD.lookup (#inDiscard s) suit))
       suits))) ^ "\n" ^
     "Hands:\n" ^ (String.concatWith "\n\n" (map handToString (#hands s))) ^ "\n" ^
     (case #turnsLeft s of
           NONE => ""
         | SOME n => (Int.toString n) ^ " turns remain.\n")))

  (* TODO *)
  fun newGame () =
  let
    val newState = newGameState ()
    val [hand,hand'] = #hands newState
  in
    printState newState
  end

end
