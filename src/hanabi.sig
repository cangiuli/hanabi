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
  (* Played has arguments: (index of card, card, successfulness (false=bomb),
   * clues given on that card, whether a hint was received as result of that play) *)
  datatype play = Discarded of int * card * info list
                | Played of int * card * bool * info list * bool
                | HintedSuit of player * suit * int list
                | HintedRank of player * rank * int list
  datatype turns = Deck of int | Turns of int (* deck size or turns remaining *)

  structure SD : DICT where type key = suit

  (* New cards and new info are always prepended. *)
  type state = {hints : int,
                fuses : int,
                clues : info list list,
                hands : (card * info list) list list,
                log : int -> (player * play) list,
                turns : turns,
                turnNumber : int,
                inPlay : rank SD.dict,
                inDiscard : rank list SD.dict}

  val newGame : (int -> state -> action) list -> int
  val newGames : int -> (int -> state -> action) list -> int list
  val newGamesAllPlayers : int -> (int -> state -> action) -> unit

end
