structure Environment = struct
  local
    open Basis
  in
    fun new_scope () = Scope.new builtins (Error o NameError)

    exception EvalFailure of AST.exp * error

    fun eval scope term = (case term of
      AST.EIdent i => Scope.get scope i
    | AST.EInt i => Int i
    | AST.EStr s => Str s
    | AST.EDot (e,s) => get_attribute (eval scope e) s
    | AST.ESub (e,i) => apply scope (get_attribute (eval scope e) "__getitem__") (map (eval scope) i)
    | AST.EBinop (b, x, y) => apply scope (get_attribute (eval scope x) (AST.binop_magic b)) [eval scope y]
    | AST.EApp (f,e) => apply scope (eval scope f) (map (eval scope) e)
    | AST.EList es => List (ref (map (eval scope) es))
    | AST.ETuple es => Tuple (map (ref o eval scope) es)
    | AST.ERaise e => invoke scope (eval scope e) "__raise__"
    | AST.ELam (i,e) => mkfun1s "<lambda>" (fn (s,x) => let val s = Scope.expand s in Scope.set s i x; eval s e end)
    | AST.ESeq (e1,e2) => (eval scope e1 ; eval scope e2)
    | AST.ETry (e,ex,i,n) => (eval scope e handle EvalFailure (_,UserError (err,msg)) =>
      if instanceof err (cast_class (Scope.get scope ex)) then eval (let val scope = Scope.expand scope in Scope.set scope i err; scope end) n else raise Error (UserError (err,msg)))
    ) handle
      EvalFailure d => raise EvalFailure d
    | Error (AttributeError (obj,s)) => invoke scope (apply scope Except.attribute_exception [obj,Str s]          ) "__raise__"
    | Error (IndexError (v,i,a,b))   => invoke scope (apply scope Except.index_exception [Str v,Int i,Int a,Int b]) "__raise__"
    | Error (NameError n)            => invoke scope (apply scope Except.name_exception [Str n]                   ) "__raise__"
    | Error (ParseError s)           => invoke scope (apply scope Except.parse_exception [Str s]                  ) "__raise__"
    | Error (TypeError s)            => invoke scope (apply scope Except.type_exception [Str s]                   ) "__raise__"
    | Error (ValueError (obj,s))     => invoke scope (apply scope Except.value_exception [obj,Str s]              ) "__raise__"
    | Error (UserError (obj,s))      => raise EvalFailure(term,UserError (obj,s))
    | ex => raise ex
    fun run scope term = case term of
      AST.DExp e => (eval scope e; ())
    | AST.DAssign (n,e) => Scope.set scope n (eval scope e)
    | AST.DSet _ => raise Fail "Setting is Unimplemented"
    | AST.DWrite (i,l,e) => let val e = eval scope e val i = eval scope i val l = map (eval scope) l in apply scope (get_attribute i "__setitem__") (l@[e]); () end
    | AST.DIf (c,d_if,d_elif,d_else) => if cast_bool scope (eval scope c) then run_all scope d_if else
      ( case d_elif of
        [] => Option.app (run_all scope) d_else
      | (c,d_if)::d_elif => run scope (c,d_if,d_elif,d_else)
      )
    | AST.DTry (e,ex,i,n,f) => (
      ( handle )
    )

    fun eval_exp scope tokens = run scope (Parse.parse tokens)

    fun eval_line scope source = eval_exp scope (Tokenize.tokenize source)

    fun run_line scope code = print
      (if List.all Char.isSpace (String.explode code) then "" else
        ((to_string scope (eval_line scope code)
          handle EvalFailure (term,err) => "Traceback:\n\t" ^ AST.exp_to_string term ^ "\n" ^ error_to_string err
            | Error err => "Traceback:\n" ^ error_to_string err) ^ "\n"))
    
    val () = Std.Table.set builtins "eval" (mkfun1s "eval" (fn (s,obj) => eval_line s (cast_str s obj)))
  end
end