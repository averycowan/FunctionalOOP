structure Parse = struct
  local
    open Std
    open AST
    open Tokenize
  in
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
    fun parse_exp l s k = parse_exp_seq l s k
    and parse_exp_basic l (s : exp * token list -> 'z) k =
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
      k))))
    and parse_exp_raise l s k =
      parse_exp_basic l (fn (e,l) =>
        parse_sym TRaise l (fn l =>
          s(ERaise e,l)) (fn () =>
        s(e,l)))
      k
    and parse_exp_times l (s : exp * token list -> 'z) k =
      parse_exp_raise l (fn (e,l) =>
        parse_sym TTimes l (fn l =>
          parse_exp_times l (fn (d,l) =>
            s(EBinop(BTimes,d,e),l))
          k) (fn () =>
        s(e,l)))
      k
    and parse_exp_plus l (s : exp * token list -> 'z) k =
      parse_exp_times l (fn (e,l) =>
        parse_sym TPlus l (fn l =>
          parse_exp_plus l (fn (d,l) =>
            s(EBinop(BPlus,d,e),l))
          k) (fn () =>
        s(e,l)))
      k
    and parse_exp_gt l s k =
      parse_exp_plus l (fn (e,l) =>
        parse_sym TGreaterThan l (fn l =>
          parse_exp_gt l (fn (d,l) =>
            s(EBinop(BGreaterThan,d,e),l))
          k) (fn () =>
        s(e,l)))
      k
    and parse_exp_eql l s k =
      parse_exp_gt l (fn (e,l) =>
        parse_sym TEquals l (fn l =>
          parse_exp_eql l (fn (d,l) =>
            s(EBinop(BEquals,d,e),l))
          k) (fn () =>
        s(e,l)))
      k
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
    
    fun parse tokens =
      let
        val stream = List.rev tokens
        val program = parse_program stream SOME (fn () => NONE)
        fun k () = Basis.parse_error (String.concatWithMap " " token_to_string tokens)
      in
        Std.getopt program k
      end
  end
end