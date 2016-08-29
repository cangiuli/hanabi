signature HANABI =
sig

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

  structure SD : DICT where type key = suit

  (* When a player receives the state, #hands contains all other players'
   * cards with clues, while #clues contains their own clues only. *)
  type state = {hints : int,
                fuses : int,
                clues : info list list,
                hands : (card * info list) list list,
                turnsLeft : int option,
                inDeck : card list,
                inPlay : rank SD.dict,
                inDiscard : rank list SD.dict}

  val score : state -> int

  val newGame : (state -> action) list -> int

end
