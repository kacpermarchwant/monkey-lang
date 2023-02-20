open Core

type token_type =
  | Illegal
  | Eof
  | Identifier
  | Num
  | Assign
  | Plus
  | Minus
  | Bang
  | Asterisk
  | Slash
  | Lt
  | Gt
  | Eq
  | Not_eq
  | Comma
  | Semicolon
  | L_paren
  | R_paren
  | L_brace
  | R_brace
  | Function
  | Let
  | True
  | False
  | If
  | Else
  | Return
[@@deriving sexp]

type literal = Literal of string [@@deriving sexp]
type t = { token_type : token_type; literal : literal } [@@deriving sexp]

(* WARNING: I allow illegal states here, but lets just ignore it for now  *)
let create token_type ch : t = { token_type; literal = Literal ch }

let match_identifier identifier : token_type =
  match identifier with
  | "fn" -> Function
  | "let" -> Let
  | "true" -> True
  | "false" -> False
  | "if" -> If
  | "else" -> Else
  | "return" -> Return
  | _ -> Identifier