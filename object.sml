structure Class :> sig
    eqtype t

    val None : t
    val Int : t
    val Str : t
    val List : t
    val Tuple : t
    val Bool : t
    (* val Bytes : t *)
    (* val Float : t *)
    val Function : t
    val Class : t
    val Iter : t

    val new : string -> t

    val primitive : t -> bool
    
    val toString : t -> string
    val equal : t -> t -> bool
    val compare : t -> t -> order

    val extend : t -> string -> t
    val subclass : t -> t -> bool

  end = struct
    open Std
    datatype t = C of int * string * t option
    fun idx (C(x,_,_)) = x
    fun name (C(_,x,_)) = x
    fun parent (C(_,_,x)) = x

    val c = ref ~1
    fun new name = ((c := 1 + !c); C(!c, name,NONE))

    val None = new "NoneType"
    val Int = new "int"
    val Str = new "str"
    val List = new "list"
    val Tuple = new "tuple"
    val Bool = new "bool"
    (* val Bytes = new "bytes" *)
    (* val Float = new "float" *)
    val Function = new "function"
    val Class = new "class"
    val Iter = new "iter"

    fun primitive x = idx x <= idx Function

    fun toString (C(_,s,_)) = s
    fun equal (C(x, _, _)) (C (y, _, _)) = x = y
    fun compare (C(x, _, _)) (C (y, _, _)) = Int.compare (x,y)

    fun extend p name = ((c := 1 + !c); C(!c, name, SOME p))
    fun subclass (c1 as C(x, _, p)) (c2 as C (y, _, _)) = x = y orelse oexists (flip subclass c2) p
  end

structure Object = struct
  datatype object =
    None
  | Int of int
  | Str of string
  | Bool of bool
  | Function of string * (object Scope.scope -> object list -> object)
  | List of object list ref
  | Tuple of object ref list
  | Class of Class.t * object
  | Object of object Std.Table.table
  type scope = object Scope.scope
end