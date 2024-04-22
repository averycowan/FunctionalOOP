(* structure Parse = struct
  open Std
  open AST

  fun char_to_string c = String.implode [c]
  val char_is_punct = fn #"_" => false | c => Char.isPunct c
  val parse_error = Object.parse_error

  datatype token =
    TIdent of string
  | TInt of int
  | TStr of string
  | TNewLine
  | TDot
  | TSemicolon
  | TComma
  | TParenL
  | TParenR
  | TBracketL
  | TBracketR
  | TPlus
  | TTimes
  | TEquals
  | TGreaterThan
  | TAssign
  | TLambda
  | TColon
  | TTry
  | TExcept
  | TRaise

  datatype sourcecode =
    Source of string
  | Token of token

  local
    datatype special_character_state = Code | Quote | Special | Symbol

    fun parse_special (#"\"",(Code,acc,s)) = (Quote,[],Source(String.implode(List.rev acc))::s)
      | parse_special (#"\"",(Quote,acc,s)) = (Code,[],Token(TStr(String.implode(List.rev acc)))::s)
      | parse_special (#"\"",(Special,acc,s)) = (Quote,#"\""::acc,s)
      | parse_special (#"\"",(Symbol,acc,s)) = (Quote,[],Source(String.implode(List.rev acc))::s)

      | parse_special (#"\\"(*"( *),(Quote,acc,s)) = (Special,acc,s)
      | parse_special (#"\\"(*"( *),(Special,acc,s)) = (Quote,#"\\"(*"( *)::acc,s)
      | parse_special (#"n",(Special,acc,s)) = (Quote,#"\n"::acc,s)
      | parse_special (#"t",(Special,acc,s)) = (Quote,#"\t"::acc,s)
      | parse_special (#"b",(Special,acc,s)) = (Quote,#"\b"::acc,s)
      | parse_special (#"\n",(Special,acc,s)) = (Quote,acc,s)
      | parse_special (c, (Special,_,_)) = parse_error ("Character '" ^ char_to_string c ^ "' is not a special character")

      | parse_special (c,(Quote,acc,s)) = (Quote,c::acc,s)

      | parse_special (#"\n",(Code,acc,s)) = (Code,[],(*Token TNewLine::*)Source(String.implode(List.rev(acc)))::s)
      | parse_special (#"\n",(Symbol,acc,s)) = (Code,#" "::acc,s)

      | parse_special (c as (#"." | #"(" | #")" | #"[" | #"]" | #"{" | #"}"),(_,acc,s)) = (Code,(#" ")::c::(#" ")::acc,s)

      | parse_special (c,(Code,acc,s)) = if char_is_punct c then (Symbol,c::(#" ")::acc,s) else (Code,c::acc,s)
      | parse_special (c,(Symbol,acc,s)) = if char_is_punct c then (Symbol,c::acc,s) else (Code,c::(#" ")::acc,s)
    
    fun output_state ((Code|Symbol),acc,s) = List.rev (Source(String.implode(List.rev acc)) :: s)
      | output_state ((Quote|Special),acc,s) = parse_error ("Unterminated quote while parsing")
  in
    fun parse_source code = output_state (foldl parse_special (Code,[],[]) (String.explode code))
  end

  fun token_to_string t = case t of
    TIdent i => i
  | TInt i => "INT_"^Int.toString i
  | TStr i => "STR\"" ^ i ^ "\""
  | TDot => "DOT"
  | TNewLine => "NEWLINE"
  | TSemicolon => "SEMICOLON"
  | TComma => "COMMA"
  | TParenL => "PARENL"
  | TParenR => "PARENR"
  | TBracketL => "BRACKETL"
  | TBracketR => "BRACKETR"
  | TPlus => "PLUS"
  | TTimes => "TIMES"
  | TEquals => "EQUALS"
  | TGreaterThan => "GREATERTHAN"
  | TAssign => "ASSIGN"
  | TLambda => "LAMBDA"
  | TColon => "COLON"
  | TTry => "TRY"
  | TExcept => "EXCEPT"
  | TRaise => "RAISE"

  fun parse_token s = case s of
    "." => TDot
  | ";" => TSemicolon
  | "," => TComma
  | "(" => TParenL
  | ")" => TParenR
  | "[" => TBracketL
  | "]" => TBracketR
  | "+" => TPlus
  | "*" => TTimes
  | "==" => TEquals
  | ">" => TGreaterThan
  | "=" => TAssign
  | ":" => TColon
  | "lambda" => TLambda
  | "try" => TTry
  | "except" => TExcept
  | "raise" => TRaise
  | _ =>
    if String.isPrefix "\"" s andalso String.isSuffix "\"" s then
      if String.size s < 2 then parse_error "Unmatched quote in parsing"
      else TStr (String.substring (s, 1, String.size s - 2))
    else (case Int.fromString s of SOME i => TInt i | NONE =>
      TIdent s
    )
  (* fun tokenize_char (t as (#"_" | #"\"" | #"=")) = char_to_string t
    | tokenize_char #"\n" = ""
    | tokenize_char x = if Char.isPunct x then " " ^ char_to_string x ^ " " else char_to_string x *)

  fun tokenize_code (Source code) = map parse_token (String.tokens Char.isSpace code)
    | tokenize_code (Token t) = [t]
  
  fun tokenize line = List.concatMap tokenize_code (parse_source line)

  fun parse_eof [] s k = s()
    | parse_eof _ s k = k()
  fun parse_ident (TIdent i::ts) s k = s (i,ts)
    | parse_ident _ s k = k()
  fun parse_int (TInt i::ts) s k = s (i,ts)
    | parse_int _ s k = k()
  fun parse_str (TStr i::ts) s k = s (i,ts)
    | parse_str _ s k = k()
  fun parse_binop (TPlus::ts) s k = s(TPlus,ts)
    | parse_binop (TTimes::ts) s k = s(TTimes,ts)
    | parse_binop (TEquals::ts) s k = s(TEquals,ts)
    | parse_binop (TGreaterThan::ts) s k = s(TGreaterThan,ts)
    | parse_binop _ s k = k()
  fun parse_sym (m : token) (t::ts) s k = if m=t then s ts else k()
    | parse_sym m _ s k = k()
  fun mk_parse_infix
    (token : token)
    (exp : exp -> exp -> exp)
    (parse_next : token list -> (exp * token list -> 'z) -> (unit -> 'z) -> 'z)
    (l : token list)
    (s : exp * token list -> 'z)
    (k : unit -> 'z) : 'z =
    let
      open Std.OL
      fun aux acc =
        (fn l => parse_next l
          (fn (e,l) => parse_sym token l
            (fn l => aux (cons e acc) l)
          (fn () => s(foldl exp Fn.id (rev (cons e acc)),l)))
        k)
    in
      parse_next l (fn (e,l) =>
        parse_sym token l (fn l =>
          aux (sing e) l) (fn () =>
        s(e,l)))
      k
    end
  fun mk_parse_binop token b parse_next =
    mk_parse_infix token (fn x => fn y => EBinop(b,x,y)) parse_next 
  fun parse_exp l s k = parse_exp_seq l s k
  (* and parse_exp_basic l (s : exp * token list -> 'z) k =
    parse_ident l (fn (i,l) =>
      parse_sym TDot l (fn l =>
        parse_exp_basic l (fn (e,l) =>
          s(EDot(e,i),l))
        k) (fn () =>
      s(EIdent i,l))) (fn () =>
    parse_list l (fn (i,l) =>
      parse_exp_basic l (fn (e,l) =>
        s(ESub(e,i),l)) (fn () =>
      s(EList i,l))) (fn () =>
    parse_tuple l (fn (i,l) =>
      parse_exp_basic l (fn (e,l) =>
        s(EApp(e,i),l)) (fn () =>
      s(ETuple i,l))) (fn () =>
    parse_int l (fn (i,l) =>
      s(EInt i,l)) (fn () =>
    parse_str l (fn (i,l) =>
      s(EStr i,l))
    k)))) *)
  (* and parse_exp_raise l s k =
    parse_exp_basic l (fn (e,l) =>
      parse_sym TRaise l (fn l =>
        s(ERaise e,l)) (fn () =>
      s(e,l)))
    k *)
  and parse_list l s k =
    parse_sym BRACKETL (fn l =>
      parse_sym BRACKETR (fn l =>
        s([],l)
      ) (fn () =>
        parse_exp l (fn (e,l) =>
          parse_list_follow l (fn (r,l) =>
            s(e::r,l)
          )
        )
      )
    )
  and parse_exp_times l = mk_parse_binop TTimes BTimes parse_exp_raise l
  and parse_exp_plus l = mk_parse_binop TPlus BPlus parse_exp_times l
  and parse_exp_gt l = mk_parse_binop TGreaterThan BGreaterThan parse_exp_plus l
  and parse_exp_eql l = mk_parse_binop TEquals BEquals parse_exp_gt l
  (* and parse_exp_times l (s : exp * token list -> 'z) k =
    parse_exp_raise l (fn (e,l) =>
      parse_sym TTimes l (fn l =>
        parse_exp_times l (fn (d,l) =>
          s(EBinop(BTimes,d,e),l))
        k) (fn () =>
      s(e,l)))
    k *)
  (* and parse_exp_plus l (s : exp * token list -> 'z) k =
    parse_exp_times l (fn (e,l) =>
      parse_sym TPlus l (fn l =>
        parse_exp_plus l (fn (d,l) =>
          s(EBinop(BPlus,d,e),l))
        k) (fn () =>
      s(e,l)))
    k *)
  (* and parse_exp_gt l s k =
    parse_exp_plus l (fn (e,l) =>
      parse_sym TGreaterThan l (fn l =>
        parse_exp_gt l (fn (d,l) =>
          s(EBinop(BGreaterThan,d,e),l))
        k) (fn () =>
      s(e,l)))
    k *)
  (* and parse_exp_eql l s k =
    parse_exp_gt l (fn (e,l) =>
      parse_sym TEquals l (fn l =>
        parse_exp_eql l (fn (d,l) =>
          s(EBinop(BEquals,d,e),l))
        k) (fn () =>
      s(e,l)))
    k *)
  and parse_exp_lam_follow e (l as b) s k =
    parse_sym TColon l (fn l =>
      parse_ident l (fn (i,l) =>
        parse_sym TLambda l (fn l =>
          parse_exp_lam_follow (ELam (i,e)) l s k) (fn () =>
        s(e,b)))
      k) (fn () =>
    s(e,l))
  and parse_exp_lam l s k =
    parse_exp_eql l (fn (e,l as b) =>
      parse_sym TColon l (fn l =>
        parse_ident l (fn (i,l) =>
          parse_sym TLambda l (fn l =>
            parse_exp_lam_follow (ELam (i,e)) l s k) (fn () => 
          s(e,b)))
        k) (fn () =>
      s(e,l)))
    k
  and parse_exp_try_follow n l s k =
    parse_sym TColon l (fn l =>
      parse_ident l (fn (i,l) =>
        parse_ident l (fn (ex,l) =>
          parse_sym TExcept l (fn l =>
            parse_exp l (fn (e,l) =>
              parse_sym TTry l (fn l =>
                parse_exp_lam_follow (ETry (e,ex,i,n)) l s k)
              k)
            k)
          k)
        k)
      k) (fn () =>
    s(n,l))
  and parse_exp_try l s k =
    parse_exp_lam l (fn (n,l) =>
      parse_sym TColon l (fn l =>
        parse_ident l (fn (i,l) =>
          parse_ident l (fn (ex,l) =>
            parse_sym TExcept l (fn l =>
              parse_exp l (fn (e,l) =>
                parse_sym TTry l (fn l =>
                  parse_exp_try_follow (ETry (e,ex,i,n)) l s k)
                k)
              k)
            k)
          k)
        k) (fn () =>
      s(n,l)))
    k
  and parse_exp_seq l s k =
    parse_exp_try l (fn (e,l) =>
      parse_sym TSemicolon l (fn l =>
        parse_exp_seq l (fn (d,l) =>
          s(ESeq(d,e),l))
        k) (fn () =>
      s(e,l)))
    k
  and parse_list (l : token list) (s : exp list * token list -> 'z) (k : unit -> 'z) : 'z =
    parse_sym TBracketR l (fn l =>
      parse_sym TBracketL l (fn l =>
        s([],l)) (fn () =>
      parse_exp l (fn (e,l) =>
        parse_list_follow l (fn (f,l) =>
          s(List.rev (e::f),l))
        k) 
      k))
    k
  and parse_list_follow l s k =
    parse_sym TBracketL l (fn l =>
      s([],l)) (fn () =>
    parse_sym TComma l (fn l =>
      parse_exp l (fn (e,l) =>
        parse_list_follow l (fn (f,l) =>
          s(e::f,l))
        k)
      k)
    k)

  and parse_tuple l s k =
    parse_sym TParenR l (fn l =>
      parse_sym TParenL l (fn l =>
        s([],l)) (fn () =>
      parse_exp l (fn (e,l) =>
        parse_tuple_follow l (fn (f,l) =>
          s(List.rev (e::f),l))
        k) 
      k))
    k
  and parse_tuple_follow l s k =
    parse_sym TParenL l (fn l =>
      s([],l)) (fn () =>
    parse_sym TComma l (fn l =>
      parse_exp l (fn (e,l) =>
        parse_tuple_follow l (fn (f,l) =>
          s(e::f,l))
        k)
      k)
    k)
  fun parse_program l s k =
    parse_exp l (fn (r,l) =>
      parse_sym TAssign l (fn l =>
        parse_list l (fn (i,l) =>
          parse_exp l (fn (e,l) =>
            parse_eof l (fn () =>
              s(DWrite(e,i,r)))
            k)
          k) (fn () =>
        parse_ident l (fn (i,l) =>
          parse_eof l (fn () =>
            s(DAssign(i,r)))
          k)
        k)) (fn () =>
      parse_eof l (fn () =>
        s(DExp r))
      k))
    k 
  
  fun parse input =
    let
      val tokens = tokenize input
      val stream = List.rev tokens
      val program = parse_program stream SOME (fn () => NONE)
      fun k () = Object.parse_error (String.concatWithMap " " token_to_string tokens)
    in
      Std.getopt program k
    end
end *)