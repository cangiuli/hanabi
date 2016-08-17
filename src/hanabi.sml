structure Hanabi =
struct

  datatype suit = White | Yellow | Green | Blue | Red | Rainbow
  (* card rank must be in [1,5] *)
  type card = suit * int

  (* for now, only supporting 6-suit Hanabi *)
  val deck =
    let fun makeSuit (s : suit) = map (fn x => (s,x)) [1,1,1,2,2,3,3,4,4,5]
    in List.concat (map makeSuit [White,Yellow,Green,Blue,Red,Rainbow])
    end

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

  val shuffledDeck =
    let val deckArr = Array.fromList deck
    in shuffle (deckArr, 59); Array.foldr (op ::) [] deckArr
    end

  val _ = print ((Int.toString (length shuffledDeck)) ^ "\n");

end
