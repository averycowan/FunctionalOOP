(* signature PARSE_ARGS = sig
end

functor Parser(ParseArgs : PARSE_ARGS) = struct
  type token = ParseArgs.token
  type state_e = ParseArgs.expstate
  type state_el = ParseArgs.expliststate
  type state_d = ParseArgs.decstate
  type state_dl = ParseArgs.decliststate
  type exp = ParseArgs.exp
  type dec = ParseArgs.dec
  datatype path =
    
  type content =
    Token of token
  | StateE of state_e
  | StateEL of state_el
  | StateD of state_d
  | StateDL of state_dl
  type content =
    Token of token
  | StateE of state_e
  | StateEL of state_el
  | StateD of state_d
  | StateDL of state_dl
  type 'a content_action =
    AToken of token 
  | AStateE of state_e * (exp -> 'a)
  | AStateEL of state_el * (exp list -> 'a)
  | AStateD of state_d * (dec -> 'a)
  | AStateDL of state_dl * (dec list -> 'a)
  type path = content list * 
end *)