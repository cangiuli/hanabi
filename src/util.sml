(* Miscellaneous functions not particularly related to Hanabi. *)

structure Util =
struct

  (* splitNth (xs,n) = (List.nth(xs,n), List.take (xs,n) @ List.drop (xs,n+1)) *)
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

  (* count f xs = length (filter f xs) *)
  fun count (f : 'a -> bool) (xs : 'a list) : int =
    foldl (fn (x,n) => if f x then n+1 else n) 0 xs

  (* revFind f xs = find f (rev xs) *)
  fun revFind (f : 'a -> bool) (xs : 'a list) : 'a option =
    if null xs then NONE else
      case revFind f (tl xs) of
           NONE => if f (hd xs) then SOME (hd xs) else NONE
         | SOME y => SOME y

  (* Applies f to each element from left to right, until f x is true. Returns
   * SOME i if such an x exists at index i, otherwise it returns NONE. *)
  fun findIndex (f : 'a -> bool) (xs : 'a list) : int option =
  let
    fun loop xs i =
      case xs of
           [] => NONE
         | x::xs' => if f x then SOME i else loop xs' (i+1)
  in
    loop xs 0
  end

  (* revFindIndex f xs = findIndex f (rev xs) *)
  fun revFindIndex (f : 'a -> bool) (xs : 'a list) : int option =
    if null xs then NONE else
      case revFindIndex f (tl xs) of
           NONE => if f (hd xs) then SOME 0 else NONE
         | SOME i => SOME (i+1)

  (* Returns f v if opt is SOME v; otherwise it returns NONE. *)
  fun maybe (def : 'b) (f : 'a -> 'b) (opt : 'a option) : 'b =
    Option.getOpt (Option.map f opt, def)

  (* Returns v if opt is SOME v; otherwise it returns f (). *)
  fun otherwise (opt : 'a option, f : unit -> 'a) : 'a =
    case opt of
         SOME v => v
       | NONE => f ()

  (* checks whether x is in l *)
  fun elem (l : ''a list) (x : ''a) : bool =
  List.exists (fn y => y = x) l

end
