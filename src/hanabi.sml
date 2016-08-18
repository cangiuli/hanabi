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
  datatype info = IsSuit of suit | NotSuit of suit | IsRank of rank | NotRank of rank
  type state = {hints : int,
                fuses : int,
                clues : info list,
                hands : (card * info list) list,
                inDeck : card list,
                inPlay : (suit * rank) list,
                inDiscard : (suit * rank list) list}

  fun suitColor s = case s of
                         White => Ansi.bright_white
                       | Yellow => Ansi.bright_yellow
                       | Green => Ansi.bright_green
                       | Blue => Ansi.bright_blue
                       | Red => Ansi.bright_red
                       | Rainbow => Ansi.dark_gray

  fun cardToString (s,n) = Ansi.colorStr (Int.toString n) (suitColor s)

  fun cardsToString (cs : card list) = String.concatWith " " (map cardToString cs)

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
      fun makeSuit (s : suit) = map (fn x => (s,x)) [1,1,1,2,2,3,3,4,4,5]
      val deck = Array.fromList
        (List.concat (map makeSuit [White,Yellow,Green,Blue,Red,Rainbow]))
    in
      shuffle (deck, 59); Array.foldr (op ::) [] deck
    end

  (* TODO *)
  fun isGameOver (s : state) : bool = false

  (* TODO *)
  fun enact (a : action, s : state) : state = s

  (* TODO *)
  fun gameLoop (s : state) : state = s

  (* TODO *)
  fun newGame () =
  let
    val deck = newDeck ()
    val hand = List.take (deck,5)
    val hand' = List.take (List.drop (deck,5),5)
  in
    print ((cardsToString hand) ^ "\n");
    print ((cardsToString hand') ^ "\n")
  end

end
