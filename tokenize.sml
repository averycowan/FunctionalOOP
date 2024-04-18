structure Tokenize = struct

  fun char_to_string c = String.implode [c]
  val char_is_punct = fn #"_" => false | c => Char.isPunct c
  val parse_error = Basis.parse_error

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
    open Std

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
  end
end