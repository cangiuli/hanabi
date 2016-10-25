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

  (* Applies f to each element from left to right, returning the indices where f is true *)
  fun findIndices (f : 'a -> bool) (xs : 'a list) : int list =
  let
    fun loop xs i =
      case xs of
           [] => []
         | x::xs' => if f x then i::loop xs' (i+1) else loop xs' (i+1)
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

  (* maps f to the nth element in l. Returns l if n >= len l *)
  fun mapAt (f : 'a -> 'a) (l : 'a list) (n : int) : 'a list =
    case (l,n) of
         ([],_) => []
       | (h::t,0) => f h::t
       | (h::t,n) => h::mapAt f t (n-1)

  (* inserts x before the nth element. Appends x if n >= len l *)
  fun insert (x : 'a) (l : 'a list) (n : int) : 'a list =
    case (l,n) of
         (l,0) => l
       | ([],_) => [x]
       | (h::t,n) => h::insert x t (n-1)

  (* Returns true iff x is in l. *)
  fun elem (l : ''a list) (x : ''a) : bool =
  List.exists (fn y => y = x) l

  (* find the (smallest) index where f is maximal. Raises an error if l is empty *)
  fun findMaxIndex (f : 'a -> int) (l : 'a list) =
  let
    fun loop (l' : 'a list) (max : int) (maxInd : int) (currInd : int) =
    case l' of [] => maxInd | x::xs =>
      let val n = f x in
        if n > max
        then loop xs n currInd (currInd + 1)
        else loop xs max maxInd (currInd + 1)
      end
  in
    case l of
         [x] => 0 (* no need to call f if the list is a singleton *)
       | x::xs => loop xs (f x) 0 1
  end

  (* revFindMaxIndex f l = findMaxIndex f (rev l) *)
  fun revFindMaxIndex (f : 'a -> int) (l : 'a list) =
  let
    fun loop (l' : 'a list) (max : int) (maxInd : int) (currInd : int) =
    case l' of [] => maxInd | x::xs =>
      let val n = f x in
        if n >= max
        then loop xs n currInd (currInd + 1)
        else loop xs max maxInd (currInd + 1)
      end
  in
    case l of
         [x] => 0 (* no need to call f if the list is a singleton *)
       | x::xs => loop xs (f x) 0 1
  end

  fun mean (is : int list) : real =
    real (foldr (fn (i, j) => i + j) 0 is) / real (length is)

  fun stdDev (is : int list) : real =
  let
    val m = mean is
    val n = length is
    val error = foldr (fn (i, j) => (real i - m) * (real i - m) + j) 0.0  is
  in
    Math.sqrt (error / real ((n - 1) * n))
  end

  (* find the value in l where f is maximal *)
  fun findMax (f : 'a -> int) (l : 'a list) =
  let
    fun loop (l' : 'a list) (max : int) =
    case l' of [] => max | x::xs =>
      let val n = f x in
        if n > max then loop xs n else loop xs max
      end
  in
    case l of
       x::xs => loop xs (f x)
  end

  (* similar to findMax *)
  fun findMin (f : 'a -> int) (l : 'a list) =
  let
    fun loop (l' : 'a list) (min : int) =
    case l' of [] => min | x::xs =>
      let val n = f x in
        if n < min then loop xs n else loop xs min
      end
  in
    case l of
       x::xs => loop xs (f x)
  end

end
