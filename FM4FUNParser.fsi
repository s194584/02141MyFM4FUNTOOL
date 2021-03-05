// Signature file for parser generated by fsyacc
module FM4FUNParser
type token = 
  | ASSIGN
  | SKIP
  | SEP
  | IF
  | FI
  | DO
  | OD
  | THEN
  | CONS
  | LBRA
  | RBRA
  | PLUS
  | MINUS
  | MULT
  | DIV
  | POW
  | LPAR
  | RPAR
  | TRUE
  | FALSE
  | AND
  | OR
  | SAND
  | SOR
  | NEQ
  | GEQ
  | LEQ
  | EQ
  | GT
  | LT
  | NOT
  | EOF
  | VAR of (string)
  | NUM of (int)
type tokenId = 
    | TOKEN_ASSIGN
    | TOKEN_SKIP
    | TOKEN_SEP
    | TOKEN_IF
    | TOKEN_FI
    | TOKEN_DO
    | TOKEN_OD
    | TOKEN_THEN
    | TOKEN_CONS
    | TOKEN_LBRA
    | TOKEN_RBRA
    | TOKEN_PLUS
    | TOKEN_MINUS
    | TOKEN_MULT
    | TOKEN_DIV
    | TOKEN_POW
    | TOKEN_LPAR
    | TOKEN_RPAR
    | TOKEN_TRUE
    | TOKEN_FALSE
    | TOKEN_AND
    | TOKEN_OR
    | TOKEN_SAND
    | TOKEN_SOR
    | TOKEN_NEQ
    | TOKEN_GEQ
    | TOKEN_LEQ
    | TOKEN_EQ
    | TOKEN_GT
    | TOKEN_LT
    | TOKEN_NOT
    | TOKEN_EOF
    | TOKEN_VAR
    | TOKEN_NUM
    | TOKEN_end_of_input
    | TOKEN_error
type nonTerminalId = 
    | NONTERM__startstart
    | NONTERM_start
    | NONTERM_C
    | NONTERM_GC
    | NONTERM_a
    | NONTERM_b
/// This function maps tokens to integer indexes
val tagOfToken: token -> int

/// This function maps integer indexes to symbolic token ids
val tokenTagToTokenId: int -> tokenId

/// This function maps production indexes returned in syntax errors to strings representing the non terminal that would be produced by that production
val prodIdxToNonTerminal: int -> nonTerminalId

/// This function gets the name of a token as a string
val token_to_string: token -> string
val start : (FSharp.Text.Lexing.LexBuffer<'cty> -> token) -> FSharp.Text.Lexing.LexBuffer<'cty> -> (cexp) 
