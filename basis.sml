
structure Basis = struct

  open Std
  open Object
  structure T = Table
  structure D = Dict

  datatype error =
    UserError of object * string
  | ValueError of object * string
  | AttributeError of object * string
  | NameError of string
  | TypeError of string
  | IndexError of string * int * int * int
  | ParseError of string
  fun error_to_string err = case err of
    UserError (obj,str) => str
  | ValueError (obj,str) => "ValueError: " ^ str
  | AttributeError (obj,str) => "AttributeError: " ^ str
  | NameError str => "NameError: " ^ str
  | TypeError str => "TypeError: " ^ str
  | IndexError (str, i, a, b) => "IndexError: " ^ str ^ " index " ^ Int.toString i ^ " is not in the range " ^ Int.toString a ^ ".." ^ Int.toString b
  | ParseError str => "ParseError: " ^ str
  exception Error of error
  fun user_error obj str = raise Error (UserError (obj,str))
  fun value_error data = raise Error (ValueError data)
  fun attribute_error data = raise Error (AttributeError data)
  fun type_error data = raise Error (TypeError data)
  fun index_error data i a b = raise Error (IndexError (data,i,a,b))
  fun argc_error name x l = type_error ("Function " ^ name ^ " requires " ^ x ^ " argument"^(if x="1" then "" else "s")^" but " ^ Int.toString (List.length l) ^ " were provided.")

  fun magic s = "__"^s^"__"

  fun nth name (l,i) = List.nth (l,i) handle Subscript => index_error name i 0 (List.length l)
  fun update name (l,i,a) = List.update (l,i,a) handle Subscript => index_error name i 0 (List.length l)

  
  fun mkfun0c name code = Function (name, fn s => (fn []    => code s       | l => argc_error name "0" l))
  fun mkfun1c name code = Function (name, fn s => (fn [x]   => code s x     | l => argc_error name "1" l))
  fun mkfun2c name code = Function (name, fn s => (fn [x,y] => code s (x,y) | l => argc_error name "2" l))
  fun mkfunxc name x code = Function (name, fn s => (fn l => getopt (code s l) (fn () => argc_error name x l)))
  fun mkfun0s name = mkfun0c name
  fun mkfun1s name code = mkfun1c name (fn s => fn x => code (s,x))
  fun mkfun2s name code = mkfun2c name (fn s => fn (x,y) => code (s,x,y))
  fun mkfunxs name x = mkfunxc name x o Fn.curry
  fun mkfun0 name code = mkfun0c name (fn s => code())
  fun mkfun1 name = mkfun1c name o Fn.const
  fun mkfun2 name = mkfun2c name o Fn.const
  fun mkfunx name x = mkfunxc name x o Fn.const
  fun mkfunk name = mkfun0 name o Fn.const
  
  fun mw name value = "<method wrapper '" ^ name ^ "' for " ^ value ^ ">"
  fun cw f class = f ("<method wrapper '__call__' for " ^ class ^ ">")
  fun bi f name = f ("<built-id function " ^ name ^ ">")

  fun lookup data name = T.get_exn data name (Error (AttributeError (Object data,name)))
  

  (* fun instantiate_object
    (get_attribute : object -> string -> object)
    (table : object T.table)
    (data : 'a)
    (class : object)
    (fields : (string * object) list)
    (methods : (string * ('a * object -> string -> object)) list) = *)

  fun mktable
    (get_attribute : object -> string -> object)
    (data : 'a)
    (class : object)
    (tostr : scope * 'a * object -> object)
    (fields : (string * object) list)
    (methods : (string * ('a * object -> string -> object)) list) =
    let
      val rec cast_class = fn Class (t,_) => t | obj => cast_class (get_attribute obj "__classdata__")
      val rec cast_fun = fn Function (_,f) => f | obj => cast_fun (get_attribute obj "__call__")
      val classdata = cast_class class
      val constructor = cast_fun class
      val classname = Class.toString classdata
      val a = T.new ()
      val obj = Object a
      val str = mkfun0s (mw "__str__" classname) (fn s => tostr (s, data, obj))
      val dir = mkfun0 (mw "__dir__" classname) (fn () => List (ref (map Str (T.keys a))))
      val () = List.app (fn (name,field) => T.set a name field) fields
      val () = List.app (fn (name,method) => T.set a name (method (data,obj) (mw name classname))) methods
      val () = T.set a "__class__" class
      val () = T.set a "__str__" str
      val () = T.set a "__dir__" dir
    in
      a
    end

  fun mkobj 
    (get_attribute : object -> string -> object)
    (data : 'a)
    (class : object)
    (tostr : scope * 'a * object -> object)
    (fields : (string * object) list)
    (methods : (string * ('a * object -> string -> object)) list)
    : object =
    Object (mktable get_attribute data class tostr fields methods)

  fun get_attribute object name = case object of
    Object data => lookup data name
  | None => get_attribute_none name
  | Int i => get_attribute_int i name
  | Str s => get_attribute_str s name
  | Bool b => get_attribute_bool b name
  | Function (s,f) => get_attribute_fun s f name
  | List l => get_attribute_list l name
  | Tuple l => get_attribute_tuple l name
  | Class (c,f) => get_attribute_class c f name

  (* and get_attribute_obj d name = getopt (OL.find (fn t => T.get t name) d) (fn () => attribute_error(Object d, name)) *)

  and get_attribute_none name = case name of
    "__class__" => Class (Class.None, cw mkfun0 "None" (fn () => None))
  | "__str__" => mkfun0 (mw name "None") (fn () => Str "None")
  | "__eq__" => mkfun1 (mw "==" "None") (fn None => Bool true | _ => Bool false)
  | "__bool__" => mkfun0 (mw name "None") (fn () => Bool false)
  | "__dir__" => mkfun0 (mw name "None") (fn () => List (ref (map Str ["__class__","__str__","__eq__","__bool__","__dir__"])))
  | _ => attribute_error (None, name)
  
  and get_attribute_int i name = case name of
    "__class__" => Class (Class.Int, cw mkfun1c "int" (fn s => Int o cast_int s))
  | "__str__" => mkfun0 (mw name (Int.toString i)) (fn () => Str (Int.toString i))
  | "__eq__" => mkfun1 (mw "==" (Int.toString i)) (fn Int j => Bool (i = j) | _ => Bool false)
  | "__int__" => mkfun0 (mw name (Int.toString i)) (fn () => Int i)
  | "__bool__" => mkfun0 (mw name (Int.toString i)) (fn () => Bool (i <> 0))
  | "__plus__" => mkfun1c (mw "+" (Int.toString i)) (fn s => fn obj => Int (i + (cast_int s obj)))
  | "__times__" => mkfun1s (mw "*" (Int.toString i)) (fn (s,obj) => Int (i * cast_int s obj))
  | "__gt__" => mkfun1s (mw ">" (Int.toString i)) (fn (s,obj) => Bool (i > cast_int s obj))
  | "__dir__" => mkfun0 (mw name (Int.toString i)) (fn () => List (ref (map Str ["__class__","__str__","__eq__","__int__","__bool__","__plus__","__times__","__gt__","__dir__"])))
  | _ => attribute_error (Int i, name)

  and get_attribute_str i name = case name of
    "__class__" => Class (Class.Str, cw mkfun1c "str" (fn s => Str o cast_str s))
  | "__str__" => mkfun0 (mw name ("\""^i^"\"")) (fn () => Str i)
  | "__eq__" => mkfun1 (mw "==" "\""^i^"\"") (fn Str j => Bool (i = j) | _ => Bool false)
  | "__int__" => mkfun0 (mw name ("\""^i^"\"")) (fn () => case Int.fromString i of SOME i => Int i | NONE => value_error (Str i, "is not a valid integer literal"))
  | "__bool__" => mkfun0 (mw name ("\""^i^"\"")) (fn () => Bool (i <> ""))
  | "__len__" => mkfun0 (mw name ("\""^i^"\"")) (fn () => Int (String.size i))
  | "__getitem__" => mkfun1s (mw name ("\""^i^"\"")) (fn (s,idx) => Str (Char.toString (String.sub (i, cast_int s idx))))
  | "__plus__" => mkfun1s (mw "+" ("\""^i^"\"")) (fn (s,obj) => Str (i ^ cast_str s obj))
  | "__dir__" => mkfun0 (mw name ("\""^i^"\"")) (fn () => List (ref (map Str ["__class__","__str__","__eq__","__int__","__bool__","__len__","__getitem__","__plus__","__dir__"])))
  | _ => attribute_error (Str i, name)

  and get_attribute_bool b name = case name of
    "__class__" => Class (Class.Bool, cw mkfun1c "bool" (fn s => Bool o cast_bool s))
  | "__str__" => mkfun0 (mw name (if b then "True" else "False")) (fn () => Str (if b then "True" else "False"))
  | "__eq__" => mkfun1 (mw "==" (if b then "True" else "False")) (fn Bool c => Bool (b = c) | _ => Bool false)
  | "__int__" => mkfun0 (mw name (if b then "True" else "False")) (fn () => Int (if b then 1 else 0))
  | "__bool__" => mkfun0 (mw name (if b then "True" else "False")) (fn () => Bool b)
  | "__and__" => mkfun1s (mw "and" (if b then "True" else "False")) (fn (s,obj) => Bool (cast_bool s obj andalso b))
  | "__or__" => mkfun1s (mw "or" (if b then "True" else "False")) (fn (s,obj) => Bool (cast_bool s obj orelse b))
  | "__xor__" => mkfun1s (mw "xor" (if b then "True" else "False")) (fn (s,obj) => Bool (cast_bool s obj <> b))
  | "__plus__" => mkfun1s (mw "+" (if b then "True" else "False")) (fn (s,obj) => Int ((if b then 1 else 0) + cast_int s obj))
  | "__dir__" => mkfun0 (mw name (if b then "True" else "False")) (fn () => List (ref (map Str ["__class__","__str__","__eq__","__bool__","__and__","__or__","__xor__","__int__","__plus__","__dir__"])))
  | _ => attribute_error (Bool b, name)

  and get_attribute_fun n f name = case name of
    "__class__" => Class (Class.Function, cw mkfun2s "function" (fn (s,n,f) => Function (cast_str s n, cast_fun f)))
  | "__str__" => mkfun0 (mw name n) (fn () => Str n)
  | "__eq__" => mkfunk (mw "==" n) (Bool false)
  | "__call__" => Function (n,f)
  | "__dir__" => mkfun0 (mw name n) (fn () => List (ref (map Str ["__class__","__str__","__call__","__dir__"])))
  | _ => attribute_error (Function (n,f), name)
  
  and get_attribute_list l name = case name of
    "__class__" => Class (Class.List, Function (cw Fn.id "list", Fn.const (List o ref)))
  | "__str__" => mkfun0s (mw name "[...]") (fn s => Str (Std.list_to_string (to_string s) (!l)))
  | "__eq__" => mkfun1c (mw "==" "[...]") (fn s => (fn List m => Bool (ListPair.allEq (fn (x,y) => cast_bool s (call s x "__eq__" [y])) (!l,!m) handle ListPair.UnequalLengths => false) | _ => Bool false))
  | "__bool__" => mkfun0 (mw name "[...]") (fn () => Bool (List.null (!l)))
  | "__len__" => mkfun0 (mw name "[...]") (fn () => Int (List.length (!l)))
  | "__getitem__" => mkfunxc (mw name "[...]") "1" (fn s => (fn [] => NONE | [i] => SOME (nth "list" (!l, cast_int s i)) | i::is => SOME (call s (nth "list" (!l, cast_int s i)) "__getitem__" is)))
  | "__setitem__" => mkfunxc (mw name "[...]") "2" (fn s => (fn ([]|[_]) => NONE | [i,v] => (l := update "list" (!l, cast_int s i, v); SOME None) | i::is => SOME (call s (nth "list" (!l, cast_int s i)) "__setitem__" is)))
  | "__contains__" => mkfun1s (mw name "[...]") (fn (s,obj) => Bool(List.exists (cast_bool s o call s obj "__eq__" o sing) (!l)))
  | "__plus__" => mkfun1s (mw "+" "[...]") (fn (s,obj) => List (ref (!l @ (! (cast_list s obj)))))
  | "__iter__" => mkfun0 (mw name "[...]") (fn () => mkiter "list" (!l))
  | "append" => mkfun1 (mw name "[...]") (fn obj => (l := obj :: !l; None))
  | "__dir__" => mkfun0 (mw name "[...]") (fn () => List (ref (map Str ["__class__","__str__","__eq__","__bool__","__len__","__getitem__","__setitem__","__contains__","__plus__","__iter__","__dir__","append"])))
  | _ => attribute_error (List l, name)

  and get_attribute_tuple l name = case name of
    "__class__" => Class (Class.Tuple, Function (cw Fn.id "tuple", Fn.const (Tuple o map ref)))
  | "__str__" => mkfun0s (mw name "(...)") (fn s => Str ("(" ^ String.concatWithMap ", " (to_string s o !) l ^ ")"))
  | "__eq__" => mkfun1c (mw "==" "(...)") (fn s => (fn Tuple m => Bool (ListPair.allEq (fn (x,y) => cast_bool s (call s (!x) "__eq__" [!y])) (l,m) handle ListPair.UnequalLengths => false) | _ => Bool false))
  | "__bool__" => mkfun0 (mw name "(...)") (fn () => Bool (List.null l))
  | "__len__" => mkfun0 (mw name "(...)") (fn () => Int (List.length l))
  | "__getitem__" => mkfun1s (mw name "(...)") (fn (s,idx) => ! (nth "tuple" (l, cast_int s idx)))
  | "__setitem__" => mkfun2s (mw name "(...)") (fn (s,idx,value) => (nth "tuple" (l, cast_int s idx) := value; None))
  | "__contains__" => mkfun1s (mw name "(...)") (fn (s,obj) => Bool(List.exists (cast_bool s o call s obj "__eq__" o sing o !) l))
  | "__plus__" => mkfun1s (mw "+" "(...)") (fn (s,obj) => Tuple (map ref (map ! l @ map ! (cast_tuple s obj))))
  | "__iter__" => mkfun0 (mw name "(...)") (fn () => mkiter "tuple" (map ! l))
  | "__dir__" => mkfun0 (mw name "(...)") (fn () => List (ref (map Str ["__class__","__str__","__eq__","__bool__","__len__","__getitem__","__setitem__","__contains__","__plus__","__iter__","__dir__"])))
  | _ => attribute_error (Tuple l, name)

  and get_attribute_class c f name = case name of
    "__class__" => Class (Class.Class, cw mkfun1 "class" (fn obj => get_attribute obj "__class__"))
  | "__str__" => mkfun0 (mw name ("<class '" ^ Class.toString c ^ "'>")) (fn () => Str ("<class '" ^ Class.toString c ^ "'>"))
  | "__eq__" => mkfun1 (mw "==" ("<class '" ^ Class.toString c ^ "'>")) (fn Class (d,_) => Bool (Class.equal c d) | _ => Bool false)
  | "__classdata__" => Class (c,f)
  | "__call__" => f
  | "__name__" => Str (Class.toString c)
  | "__instancecheck__" => mkfun1 (mw name ("<class '" ^ Class.toString c ^ "'>")) (fn obj => Bool (Class.subclass (cast_class (get_attribute obj "__class__")) c))
  | "__subclassof__" => mkfun1 (mw name ("<class '" ^ Class.toString c ^ "'>")) (Bool o Class.subclass c o cast_class)
  | "__superclassof__" => mkfun1 (mw name ("<class '" ^ Class.toString c ^ "'>")) (Bool o flip Class.subclass c o cast_class)
  | "__dir__" => mkfun0 (mw name ("<class '" ^ Class.toString c ^ "'>")) (fn () => List (ref (map Str ["__class__","__classdata__","__call__","__name__","__str__","__eq__","__instancecheck__","__subclassof__","__superclassof__","__dir__"])))
  | _ => attribute_error (Class (c,f), name)

  and apply s object args = cast_fun object s args

  and call s object name args = apply s (get_attribute object name) args

  and invoke s object name = call s object name []

  and instanceof object class = Class.subclass (cast_class (get_attribute object "__class__")) class

  and cast_int s object = case object of
    Int i => i
  | _ => cast_int s (invoke s object "__int__")

  and cast_str s object = case object of
    Str s => s
  | _ => cast_str s (invoke s object "__str__")

  and cast_bool s object = case object of
    Bool b => b
  | _ => cast_bool s (invoke s object "__bool__")
  
  and cast_fun object = case object of
    Function (_,f) => f
  | _ => cast_fun (get_attribute object "__call__")

  and cast_list s object = case object of
    List l => l
  | _ => ref (List.unfoldl (fn iter => if cast_bool s (invoke s iter "__hasnext__") then SOME (invoke s iter "__next__", iter) else NONE) (invoke s object "__iter__"))

  and cast_tuple s object = case object of
    Tuple l => l
  | _ => cast_tuple s (invoke s object "__tuple__")

  and cast_class object = case object of
    Class (t,_) => t
  | _ => cast_class (get_attribute object "__classdata__")

  and cast_iter s object = if Class.subclass (cast_class (get_attribute object "__class__")) Class.Iter then object else invoke s object "__iter__"

  and mkiter (name : string) (l : object list) : object = mkobj get_attribute
    (ref l)
    (Class (Class.Iter, cw mkfun1c (Class.toString Class.Iter) cast_iter))
    (fn (_,_,_) => Str "<iterator for list>")
    []
    [ ("__hasnext__", fn (d,obj) => flip mkfun0 (fn () => Bool(not(List.null(!d)))))
    , ("__next__"   , fn (d,obj) => flip mkfun0 (fn () => (case !d of [] => value_error(obj,"exhausted iterator") | x::xs => (d:=xs;x))))
    ]
  
  and to_string s obj = case obj of
    Str s => "\"" ^ s ^ "\""
  | _ => cast_str s obj

  fun cast_to s class object = if instanceof object class then object else
    type_error("object " ^ to_string s object ^ " of class " ^ Class.toString (cast_class (get_attribute object "__class__")) ^ " cannot be cast to class " ^ Class.toString class)
  
  fun annot_str s obj = cast_str s (cast_to s Class.Str obj)

  val mktable :
    (* data : *) 'a ->
    (* class : *) object ->
    (* tostr : *) (scope * 'a * object -> object) ->
    (* fields : *) (string * object) list ->
    (* methods : *) (string * ('a * object -> string -> object)) list ->
    object T.table = fn data : 'a => fn class : object => mktable get_attribute data class
  
  val mkobj :
    (* data : *) 'a ->
    (* class : *) object ->
    (* tostr : *) (scope * 'a * object -> object) ->
    (* fields : *) (string * object) list ->
    (* methods : *) (string * ('a * object -> string -> object)) list ->
    object = fn data => fn class => mkobj get_attribute data class

  fun mkclass
    (classname : string)
    (construct : scope -> object list -> 'a)
    (tostr : scope * 'a * object -> object)
    (fields : (string * ('a -> object)) list)
    (methods : (string * ('a * object -> string -> object)) list)
    : object =
    let
      val classdata = Class.new classname
      fun new s args =
        let
          val class = Class (classdata,Function(cw Fn.id classname,new))
          val make = fn s => (fn data => mkobj data class tostr (map (mapr (fn f => f data)) fields) methods) o construct s
        in
          case args of [arg] => if cast_bool s (call s class "__instancecheck__" args) then arg else make s args | _ => make s args
        end
    in
      Class (classdata,Function(cw Fn.id classname,new))
    end
  fun mksubclass
    (classname : string)
    (parent : object)
    (construct : scope -> object list -> 'a * object list)
    (fields : (string * ('a * object D.dict -> object)) list)
    (methods : (string * ('a * object * object D.dict -> string -> object)) list) 
    : object =
    let
      val classdata = Class.extend (cast_class parent) classname
      fun new s args =
        let
          val class = Class (classdata,Function(cw Fn.id classname,new))
          val make = fn s => (fn (data,sargs) =>
            let
              val a = case apply s parent sargs of Object t => t | p => type_error("The parent constructor '" ^ to_string s (get_attribute parent "__call__") ^ "' of class '" ^ classname ^ "' produced an object of class " ^ Class.toString (cast_class (get_attribute p "__class__")))
              val snap = T.snapshot a
              val () = T.set a "__class__" class
              val () = List.app (fn (name,field) => T.set a name (field (data,snap))) fields
              val () = List.app (fn (name,method) => T.set a name (method (data,Object a,snap) (mw name classname))) methods
            in
              Object a
            end
            ) o construct s
        in
          case args of [arg] => if cast_bool s (call s class "__instancecheck__" args) then arg else make s args | _ => make s args
        end
    in
      Class (classdata,Function(cw Fn.id classname,new))
    end

  structure Except = struct
    val base_exception = mkclass "Exception"
    (fn s => (fn [msg] => (NONE,msg) | [err,msg] => (SOME err,msg) | l => argc_error "Exception" "1 or 2" l))
    (fn (s,(SOME err,msg),_) => Str (cast_str s err ^ ": " ^ cast_str s msg)
      | (s,(NONE,msg),obj) => Str (Class.toString (cast_class (get_attribute obj "__class__")) ^ ": " ^ cast_str s msg))
    [ ("cause",(fn (NONE,_) => None | (SOME err,_) => err))
    , ("message",snd)
    ]
    [ ("__raise__",fn (_,obj) => flip mkfun0s (fn s => user_error obj (cast_str s obj))) ]
    fun mkexcept
      (name : string)
      (numargs : string)
      (construct : scope -> object list -> ('a * object option * object) option)
      (fields : (string * ('a * object D.dict -> object)) list)
      (methods : (string * ('a * object * object D.dict -> string -> object)) list) =
      mksubclass name base_exception
      (fn s => fn l => (case construct s l of
          SOME (d,SOME src,msg) => (d,[src,msg])
        | SOME (d,NONE,msg) => (d,[msg])
        | NONE => argc_error name numargs l))
      fields methods
    val value_exception = mkexcept "ValueError" "2"
      (fn s => fn [v,msg] => SOME((v,msg),NONE,msg) | _ => NONE)
      [ ("value", fn ((v,_),_) => v) ]
      []
    val attribute_exception = mkexcept "AttributeError" "2"
      (fn s => (fn [v,a] => SOME(case cast_str s a of
            "__str__" => ((v,"__str__"),NONE,Str "An object has no attribute '__str__'")
          | a => ((v,a),NONE,Str (to_string s v ^ " has no attribute '" ^ a ^ "'")))
        | _ => NONE))
      [ ("value", fn ((v,_),_) => v)
      , ("attribute", fn ((_,a),_) => Str a)
      ]
      []
    val name_exception = mkexcept "NameError" "1"
      (fn s => (fn [n] => SOME(n, NONE, Str("name '" ^ cast_str s n ^ "' is not defined"))
        | _ => NONE))
      [ ("name", fn (n,_) => n) ]
      []
    val type_exception = mkexcept "TypeError" "1"
      (fn s => (fn [msg] => SOME((), NONE, msg) | _ => NONE))
      []
      []
    val parse_exception = mkexcept "ParseError" "1"
      (fn s => (fn [msg] => SOME((), NONE, msg) | _ => NONE))
      []
      []
    val index_exception = mkexcept "IndexError" "4"
      (fn s => (fn [v,i,a,b] => SOME((v,i,a,b),NONE,Str(cast_str s v^" index "^to_string s i^" is not in the range "^to_string s a^".."^to_string s b)) | _ => NONE))
      [ ("value",fn ((v,_,_,_),_) => v)
      , ("index",fn ((_,i,_,_),_) => i)
      , ("lower_bound",fn ((_,_,n,_),_) => n)
      , ("upper_bound",fn ((_,_,_,n),_) => n)
      ]
      []
    val key_exception = mkexcept "KeyError" "1"
      (fn s => (fn [k] => SOME(k,NONE,Str(to_string s k)) | _ => NONE))
    [ ("key",fn (k,_) => k) ]
    []
  end

  structure Lang = struct
    val except = Except.base_exception
    val map = mkclass "map"
    (fn s => (fn [f,l] => (f,cast_iter s l) | l => argc_error "map" "2" l))
    (fn (s,(f,l),_) => Str ("<map " ^ to_string s l ^ " by " ^ to_string s f ^ ">"))
    [ ("__f__",fst)
    , ("__hasnext__",fn (_,l) => get_attribute l "__hasnext__")]
    [ ("__iter__", fn (_,obj) => flip mkfunk obj)
    , ("__next__", fn ((f,l),_) => flip mkfun0s (fn s => apply s f [invoke s l "__next__"]))
    ]
    val dict = mkclass "dict"
    (fn s => (fn [] => T.new() | l => argc_error "dict" "0" l))
    (fn (s,d,_) => Str (Table.to_string (to_string s) d))
    []
    [ ("__getitem__",fn (d,_) => flip mkfun1s (fn (s,k) => getopt (T.get d (annot_str s k)) (fn () => invoke s (apply s Except.key_exception [k]) "__raise__")))
    , ("__setitem__",fn (d,_) => flip mkfun2s (fn (s,k,v) => (T.set d (annot_str s k) v; None)))
    , ("__iter__",fn (d,_) => flip mkfun0s (fn s => mkiter "dict" (D.fold (fn l => fn (k,v) => Tuple[ref (Str k),ref v]::l) [] (T.snapshot d))))
    , ("get",fn (d,obj) => flip (mkfunxc "1 or 2") (fn s => (fn 
        [k,def] => SOME(Option.getOpt(T.get d (annot_str s k),def))
      | [k]     => SOME(Option.getOpt(T.get d (annot_str s k),None))
      | _ => NONE)))
    , ("keys",fn (d,_) => flip mkfun0 (fn () => List(ref(List.map Str (T.keys d)))))
    ]
    (* val module = mkclass "module"
    (fn s => (fn
        [name] => (annot_str s name,[])
      | [name,fields] => (annot_str s name,fields)
      | _ => argc_error "module" "1" l))
    (fn (s,name,_) => Str("<module "^name^">"))
    (fn []) *)
  end
  val builtins =
    let
      val env = T.new ()
    in
      T.set env "None" None;
      T.set env "int" (Class (Class.Int, cw mkfun1s "int" (fn (s,obj) => invoke s obj "__int__")));
      T.set env "str" (Class (Class.Str, cw mkfun1s "str" (fn (s,obj) => invoke s obj "__str__")));
      T.set env "bool" (Class (Class.Bool, cw mkfun1s "bool" (fn (s,obj) => invoke s obj "__bool__")));
      T.set env "list" (Class (Class.List, cw mkfun1c "list" (fn s => List o cast_list s)));
      T.set env "tuple" (Class (Class.Tuple, cw mkfun1c "tuple" (fn s => Tuple o cast_tuple s)));
      T.set env "type" (Class (Class.Class, cw mkfun1 "class" (fn obj => get_attribute obj "__class__")));
      T.set env "True" (Bool true);
      T.set env "False" (Bool false);
      T.set env "len" (bi mkfun1s "len" (fn (s,obj) => invoke s obj "__len__"));
      T.set env "dir" (bi mkfun1s "dir" (fn (_,Object data) => List (ref (map Str (T.keys data))) | (s,prim) => invoke s prim "__dir__"));
      T.set env "print" (bi mkfun1s "print" (fn (s,obj) => (print (cast_str s obj ^ "\n"); None)));
      T.set env "ls" (bi mkfun0s "ls" (fn s => (Scope.dump s; None)));
      T.set env "Exception" Except.base_exception;
      T.set env "map" Lang.map;
      T.set env "dict" Lang.dict;
      T.set env "ValueError" Except.value_exception;
      T.set env "AttributeError" Except.attribute_exception;
      T.set env "NameError" Except.name_exception;
      T.set env "TypeError" Except.type_exception;
      T.set env "ParseError" Except.parse_exception;
      T.set env "IndexError" Except.index_exception;
      env
    end

end