open Core

type t = {
  input : string;
  mutable current_position : int;
  mutable read_position : int;
  mutable current_char : char option;
}

let peek_char lexer : char option =
  let input = lexer.input in
  let read_position = lexer.read_position in
  if read_position < String.length input then
    Some (String.get input read_position)
  else None

let read_char lexer : unit =
  lexer.current_char <- peek_char lexer;
  lexer.current_position <- lexer.read_position;
  lexer.read_position <- lexer.read_position + 1

let skip_whitespace lexer : unit =
  let rec skip_whitespace1 lexer : unit =
    let input = lexer.input in
    let current_position = lexer.current_position in
    if
      current_position < String.length input
      && Char.is_whitespace (String.get input current_position)
    then (
      read_char lexer;
      skip_whitespace1 lexer)
  in

  skip_whitespace1 lexer

let is_letter_or_dash ch = Char.is_alpha ch || Char.equal ch '_'

let read_value lexer condition : string =
  let start_idx = lexer.current_position in
  let rec find_end_idx lexer =
    match peek_char lexer with
    | None -> ()
    | Some ch ->
        if condition ch then (
          read_char lexer;
          find_end_idx lexer)
  in
  find_end_idx lexer;
  let identifier_len = lexer.current_position - start_idx + 1 in
  String.sub lexer.input ~pos:start_idx ~len:identifier_len

let read_identifier lexer : string = read_value lexer is_letter_or_dash
let read_number lexer : string = read_value lexer Char.is_digit

let next_token lexer : Token.t =
  skip_whitespace lexer;

  let token =
    match lexer.current_char with
    | None -> Token.create Eof ""
    | Some current_char -> (
        let current_char_string = Char.to_string current_char in
        match current_char with
        | '+' -> Token.create Plus current_char_string
        | '-' -> Token.create Minus current_char_string
        | '/' -> Token.create Slash current_char_string
        | '*' -> Token.create Asterisk current_char_string
        | '<' -> Token.create Lt current_char_string
        | '>' -> Token.create Gt current_char_string
        | ';' -> Token.create Semicolon current_char_string
        | ',' -> Token.create Comma current_char_string
        | '{' -> Token.create L_brace current_char_string
        | '}' -> Token.create R_brace current_char_string
        | '(' -> Token.create L_paren current_char_string
        | ')' -> Token.create R_paren current_char_string
        | '=' -> (
            match peek_char lexer with
            | Some '=' ->
                read_char lexer;
                Token.create Eq "=="
            | _ -> Token.create Assign current_char_string)
        | '!' -> (
            match peek_char lexer with
            | Some '=' ->
                read_char lexer;
                Token.create Not_eq "!="
            | _ -> Token.create Bang current_char_string)
        | ch when is_letter_or_dash ch ->
            let identifier = read_identifier lexer in
            let token_type = Token.match_identifier identifier in
            Token.create token_type identifier
        | ch when Char.is_digit ch -> Token.create Num (read_number lexer)
        | _ -> Token.create Illegal current_char_string)
  in
  read_char lexer;
  token

let create input =
  let lexer =
    { input; current_position = 0; read_position = 0; current_char = None }
  in
  read_char lexer;
  lexer