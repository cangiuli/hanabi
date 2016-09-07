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
  datatype player = Me | Other of int
  datatype play = Discarded of card
                | Played of card
                | HintedSuit of player * suit * card list
                | HintedRank of player * rank * card list
  datatype turns = Deck of int | Turns of int (* deck size or turns remaining *)

  structure SD : DICT where type key = suit

  (* New cards and new info are always prepended. *)
  type state = {hints : int,
                fuses : int,
                clues : info list list,
                hands : (card * info list) list list,
                log : int -> (player * play) list,
                turns : turns,
                inPlay : rank SD.dict,
                inDiscard : rank list SD.dict}

  val newGame : (state -> action) list -> int

end
