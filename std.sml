structure Std = struct
  open SMLofNJ.Cont
  
  abstype void = Void of void with
    fun abort (Void v) = abort v
  end

  abstype void = Void of void with
    fun abort (Void v) = abort v
  end

  val catch = fn f => callcc (abort o f o callcc o throw)

  datatype 'a self = Self of 'a self -> 'a
  val fix = fn f => f (fn x => (fn y => f(fn z => y (Self y) z)) (fn Self y => f(fn z => y (Self y) z)) x)

  val id = fn x => x
  fun flip f x y = f y x

  val fst = fn (x,_) => x
  val snd = fn (_,y) => y

  val mapl = fn f => fn (x,y) => (f x, y)
  val mapr = fn f => fn (x,y) => (x, f y)

  val maplr = fn l => fn r => fn (x,y) => (l x, r y)

  fun getopt a k = case a of SOME x => x | NONE => k ()
  fun oexists f = fn NONE => false | SOME x => f x
  fun oall f = fn NONE => true | SOME x => f x
  fun omap f = fn NONE => NONE | SOME x => f x
  fun oelim x s k = case x of SOME x => s x | NONE => k ()
  
  fun sing x = [x]

  fun list_to_string f l = "[" ^ String.concatWithMap ", " f l ^ "]"

  structure OL = struct
    datatype 'a ol = Sing of 'a | Cons of 'a * 'a ol

    fun sing x = Sing x
    fun cons x xs = Cons(x,xs)

    fun hd (Sing x | Cons (x,_)) = x
    
    fun foldr f z (Sing x) = z x
      | foldr f z (Cons (x, xs)) = f x (foldr f z xs)
    
    fun foldl f z (Sing x) = z x
      | foldl f z (Cons (x, xs)) = foldl f (f (z x)) xs
    
    fun rev l = foldl (flip cons) sing l
    
    fun to_list l = case l of Sing x => [x] | Cons (x,xs) => x :: to_list xs
  end

  structure Dict :> sig
    type 'a dict
    val empty : 'a dict
    val get : 'a dict -> string -> 'a option
    val get_exn : 'a dict -> string -> exn -> 'a
    val mem : 'a dict -> string -> bool
    val set : 'a dict -> string -> 'a -> 'a dict
    val rem : 'a dict -> string -> 'a dict
    val keys : 'a dict -> string list
    val values : 'a dict -> 'a list
    val map : ('a -> 'b) -> 'a dict -> 'b dict
    val fold : ('b -> string * 'a -> 'b) -> 'b -> 'a dict -> 'b
    val to_string : ('a -> string) -> 'a dict -> string
    end = struct
    type 'a dict = (string * 'a) list
    val empty : 'a dict = []
    fun get (d : 'a dict) key = case d of
        [] => NONE
      | (x,v)::xs => if x = key then SOME v else get xs key
    fun get_exn (d : 'a dict) key ex = case get d key of SOME v => v | NONE => raise ex
    fun mem (d : 'a dict) key = case d of [] => false | (x,_)::xs => x = key orelse mem xs key
    fun set (d : 'a dict) key value = case d of
        [] => [(key,value)]
      | (x,v)::xs => if x = key then (key,value)::xs else (x,v)::set xs key value
    fun rem (d : 'a dict) key = case d of
        [] => []
      | (x,v)::xs => if x = key then xs else (x,v)::rem xs key
    fun keys d = map fst d
    fun values d = map snd d
    fun map f d = List.map (mapr f) d
    fun fold f z [] = z
      | fold f z (x::xs) = fold f (f z x) xs
    fun to_string f d = "{" ^ String.concatWithMap ", " (fn (k,v) => k ^ ": " ^ f v) d ^ "}"
  end

  structure Table :> sig
    type 'a table
    val new : unit -> 'a table
    val get : 'a table -> string -> 'a option
    val get_exn : 'a table -> string -> exn -> 'a
    val mem : 'a table -> string -> bool
    val set : 'a table -> string -> 'a -> unit
    val set_all : 'a table -> 'a Dict.dict -> unit
    val rem : 'a table -> string -> unit
    val keys : 'a table -> string list
    val snapshot : 'a table -> 'a Dict.dict
    val to_string : ('a -> string) -> 'a table -> string
    end = struct
    type 'a table = 'a Dict.dict ref
    fun new () = ref Dict.empty
    fun get d k = Dict.get (!d) k
    fun get_exn d k ex = Dict.get_exn (!d) k ex
    fun mem d k = Dict.mem (!d) k
    fun set d k v = d := Dict.set (!d) k v
    fun set_all d vs = ignore (foldl (fn (k,vs) => (set d k (Dict.get_exn vs k Bind); Dict.rem vs k)) vs (Dict.keys vs))
    fun rem d k = d := Dict.rem (!d) k
    fun keys d = Dict.keys (!d)
    fun snapshot d = !d
    fun to_string f d = Dict.to_string f (!d)
  end

  structure Stack :> sig
    type 'a stack
    val new : unit -> 'a stack
    val get : 'a stack -> int -> 'a
    val set : 'a stack -> int -> 'a -> unit
    val add : 'a stack -> 'a -> unit
    val len : 'a stack -> int
    end = struct
    type 'a stack = int ref * 'a list ref
    fun new () = (ref 0, ref [])
    fun get (ref n, ref l) i = List.nth (l,n - i - 1)
    fun update [] _ _ = raise Subscript
      | update (_::xs) 0 y = y::xs
      | update (x::xs) n y = x::update xs (n-1) y
    fun set (ref n,s) i x = s := update (!s) (n-i-1) x
    fun add (n,s) x = (n := !n + 1; s := x :: !s)
    fun len (ref n,_) = n
  end

end