structure Scope :> sig
  type 'a scope
  val new : 'a Std.Table.table -> (string -> exn) -> 'a scope
  val expand : 'a scope -> 'a scope
  val get : 'a scope -> string -> 'a
  val set : 'a scope -> string -> 'a -> unit
  val dump : 'a scope -> unit
  end = struct
  open Std
  structure T = Table
  type 'a scope = 'a T.table OL.ol * 'a T.table * (string -> exn)
  fun new global name_error = (OL.Sing (T.new ()), global, name_error)
  fun get (OL.Sing s, global, name_error) name = Std.getopt (T.get s name) (fn () => T.get_exn global name (name_error name))
    | get (OL.Cons (s,ss),g,n) name = Std.getopt (T.get s name) (fn () => get (ss,g,n) name)
  fun set (scope,_,_) = Table.set (OL.hd scope)
  fun expand (scope,g,n) = (OL.Cons (T.new (), scope),g,n)
  fun dump (scope,g,n) = print (String.concatWithMap "\n" (list_to_string Fn.id o T.keys) (g :: List.rev (OL.to_list scope)) ^ "\n")
end