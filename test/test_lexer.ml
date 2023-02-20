open Core
open Monkey_lang
open Monkey_lang.Token

let testable_sexp = Alcotest.testable Sexp.pp Sexp.equal

let test_next_token () =
  let input =
    {|let five = 5;
  let ten = 10;
  
  let add = fn(x, y) {
    x + y;
  };
  
  let result = add(five, ten);
  !-/*5;
  5 < 10 > 5;
  
  if (5 < 10) {
    return true;
  } else {
    return false;
  }
  
  10 == 10;
  10 != 9;|}
  in
  let expected_tokens =
    [
      { token_type = Let; literal = Literal "let" };
      { token_type = Identifier; literal = Literal "five" };
      { token_type = Assign; literal = Literal "=" };
      { token_type = Num; literal = Literal "5" };
      { token_type = Semicolon; literal = Literal ";" };
      { token_type = Let; literal = Literal "let" };
      { token_type = Identifier; literal = Literal "ten" };
      { token_type = Assign; literal = Literal "=" };
      { token_type = Num; literal = Literal "10" };
      { token_type = Semicolon; literal = Literal ";" };
      { token_type = Let; literal = Literal "let" };
      { token_type = Identifier; literal = Literal "add" };
      { token_type = Assign; literal = Literal "=" };
      { token_type = Function; literal = Literal "fn" };
      { token_type = L_paren; literal = Literal "(" };
      { token_type = Identifier; literal = Literal "x" };
      { token_type = Comma; literal = Literal "," };
      { token_type = Identifier; literal = Literal "y" };
      { token_type = R_paren; literal = Literal ")" };
      { token_type = L_brace; literal = Literal "{" };
      { token_type = Identifier; literal = Literal "x" };
      { token_type = Plus; literal = Literal "+" };
      { token_type = Identifier; literal = Literal "y" };
      { token_type = Semicolon; literal = Literal ";" };
      { token_type = R_brace; literal = Literal "}" };
      { token_type = Semicolon; literal = Literal ";" };
      { token_type = Let; literal = Literal "let" };
      { token_type = Identifier; literal = Literal "result" };
      { token_type = Assign; literal = Literal "=" };
      { token_type = Identifier; literal = Literal "add" };
      { token_type = L_paren; literal = Literal "(" };
      { token_type = Identifier; literal = Literal "five" };
      { token_type = Comma; literal = Literal "," };
      { token_type = Identifier; literal = Literal "ten" };
      { token_type = R_paren; literal = Literal ")" };
      { token_type = Semicolon; literal = Literal ";" };
      { token_type = Bang; literal = Literal "!" };
      { token_type = Minus; literal = Literal "-" };
      { token_type = Slash; literal = Literal "/" };
      { token_type = Asterisk; literal = Literal "*" };
      { token_type = Num; literal = Literal "5" };
      { token_type = Semicolon; literal = Literal ";" };
      { token_type = Num; literal = Literal "5" };
      { token_type = Lt; literal = Literal "<" };
      { token_type = Num; literal = Literal "10" };
      { token_type = Gt; literal = Literal ">" };
      { token_type = Num; literal = Literal "5" };
      { token_type = Semicolon; literal = Literal ";" };
      { token_type = If; literal = Literal "if" };
      { token_type = L_paren; literal = Literal "(" };
      { token_type = Num; literal = Literal "5" };
      { token_type = Lt; literal = Literal "<" };
      { token_type = Num; literal = Literal "10" };
      { token_type = R_paren; literal = Literal ")" };
      { token_type = L_brace; literal = Literal "{" };
      { token_type = Return; literal = Literal "return" };
      { token_type = True; literal = Literal "true" };
      { token_type = Semicolon; literal = Literal ";" };
      { token_type = R_brace; literal = Literal "}" };
      { token_type = Else; literal = Literal "else" };
      { token_type = L_brace; literal = Literal "{" };
      { token_type = Return; literal = Literal "return" };
      { token_type = False; literal = Literal "false" };
      { token_type = Semicolon; literal = Literal ";" };
      { token_type = R_brace; literal = Literal "}" };
      { token_type = Num; literal = Literal "10" };
      { token_type = Eq; literal = Literal "==" };
      { token_type = Num; literal = Literal "10" };
      { token_type = Semicolon; literal = Literal ";" };
      { token_type = Num; literal = Literal "10" };
      { token_type = Not_eq; literal = Literal "!=" };
      { token_type = Num; literal = Literal "9" };
      { token_type = Semicolon; literal = Literal ";" };
      { token_type = Eof; literal = Literal "" };
    ]
  in
  let lexer = Lexer.create input in
  List.iter expected_tokens ~f:(fun expected_token ->
      let token = Lexer.next_token lexer in
      Alcotest.(check testable_sexp)
        "result = expected result"
        (Token.sexp_of_t expected_token)
        (Token.sexp_of_t token));

  ()

let () =
  let open Alcotest in
  run "Lexer"
    [
      ( "next_token",
        [ test_case "Parses input correctly" `Quick test_next_token ] );
    ]