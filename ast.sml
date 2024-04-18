structure AST = struct
  datatype binop = BPlus | BTimes | BEquals | BGreaterThan
    fun binop_magic BPlus = "__plus__"
      | binop_magic BTimes = "__times__"
      | binop_magic BEquals = "__eq__"
      | binop_magic BGreaterThan = "__gt__"
    fun binop_to_string b = case b of
      BPlus => "+" | BTimes => "*" | BEquals => "==" | BGreaterThan => ">"
    
    datatype exp =
      EIdent of string
    | EInt of int
    | EStr of string
    | EDot of exp * string
    | ESub of exp * exp list
    | EBinop of binop * exp * exp
    | EApp of exp * exp list
    | EList of exp list
    | ETuple of exp list
    | ELam of string * exp
    | ETry of exp * string * string * exp
    | ERaise of exp
    | ESeq of exp * exp
    datatype decl =
      DExp of exp
    | DAssign of string * exp
    | DSet of exp * string * exp
    | DWrite of exp * exp list * exp
    | DIf of exp * decl list * (exp * decl list) list * decl list option
    | DTry of decl list * string * string * decl list * decl list option

    fun exp_to_string term = case term of
        EIdent i => i
      | EInt i => Int.toString i
      | EStr i => "\"" ^ i ^ "\""
      | EDot (e,n) => "( " ^ exp_to_string e ^ " ) . " ^ n
      | ESub (e,s) => "( " ^ exp_to_string e ^ " ) [ " ^ String.concatWithMap " , " exp_to_string s ^ " ]"
      | EBinop (b,x,y) => "( " ^ exp_to_string x ^ " ) " ^ binop_to_string b ^ " ( " ^ exp_to_string y ^ " )"
      | EApp (e,s) => "( " ^ exp_to_string e ^ " ) ( " ^ String.concatWithMap " , " exp_to_string s ^ " )"
      | EList s => "[ " ^ String.concatWithMap " , " exp_to_string s ^ " ]"
      | ETuple s => "( " ^ String.concatWithMap " , " exp_to_string s ^ " )"
      | ERaise e => "raise " ^ exp_to_string e
      | ELam (i,e) => "lambda " ^ i ^ " : " ^ exp_to_string e
      | ETry (e,ex,i,n) => "try " ^ exp_to_string e ^ " except " ^ ex ^ " " ^ i ^ " : " ^ exp_to_string n
      | ESeq (x,y) => "( " ^ exp_to_string x ^ " ) ; ( " ^ exp_to_string y ^ " )"
    fun decl_to_string term = case term of
        DExp e => exp_to_string e
      | DAssign (n,e) => n ^ " = " ^ exp_to_string e
      | DSet (a,n,e) => exp_to_string a ^ " . " ^ n ^ " = " ^ exp_to_string e
      | DWrite (a,s,e) => exp_to_string a ^ " [ " ^ String.concatWithMap " , " exp_to_string s ^ " ] = " ^ exp_to_string e
      | DIf (c,d_if,d_elif,d_else) =>
          "if " ^ exp_to_string c ^ ":\n" ^
          decls_to_string d_if ^
          String.concatWithMap "\n" (fn (c,d) =>
            "\nelif " ^ exp_to_string c ^ ":\n" ^
            decls_to_string d
          ) d_elif ^ 
          (case d_else of SOME d => "\nelse:\n" ^ decls_to_string d | NONE => "")
      | DTry (t,c,i,e,f) => "try:\n" ^ decls_to_string t ^ "\nexcept " ^ c ^ " " ^ i ^ ":\n" ^ decls_to_string e ^ (case f of SOME fin => "\nfinally:\n"^decls_to_string fin | NONE => "")

    and decls_to_string ds = String.concatWithMap "\n" decl_to_string ds
end