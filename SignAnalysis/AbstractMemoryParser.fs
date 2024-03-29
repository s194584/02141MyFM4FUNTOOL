// Implementation file for parser generated by fsyacc
module AbstractMemoryParser
#nowarn "64";; // turn off warnings that type variables used in production annotations are instantiated to concrete type
open FSharp.Text.Lexing
open FSharp.Text.Parsing.ParseHelpers
# 2 "AbstractMemoryParser.fsp"

open AbstractMemoryAST

# 10 "AbstractMemoryParser.fs"
// This type is the type of tokens accepted by the parser
type token = 
  | ASSIGN
  | SEP
  | LBRA
  | RBRA
  | EOF
  | PLUS
  | MINUS
  | ZERO
  | NL
  | VAR of (string)
// This type is used to give symbolic names to token indexes, useful for error messages
type tokenId = 
    | TOKEN_ASSIGN
    | TOKEN_SEP
    | TOKEN_LBRA
    | TOKEN_RBRA
    | TOKEN_EOF
    | TOKEN_PLUS
    | TOKEN_MINUS
    | TOKEN_ZERO
    | TOKEN_NL
    | TOKEN_VAR
    | TOKEN_end_of_input
    | TOKEN_error
// This type is used to give symbolic names to token indexes, useful for error messages
type nonTerminalId = 
    | NONTERM__startstart
    | NONTERM_start
    | NONTERM_AExpr
    | NONTERM_AExprA
    | NONTERM_Ass
    | NONTERM_Assa
    | NONTERM_Arr
    | NONTERM_a

// This function maps tokens to integer indexes
let tagOfToken (t:token) = 
  match t with
  | ASSIGN  -> 0 
  | SEP  -> 1 
  | LBRA  -> 2 
  | RBRA  -> 3 
  | EOF  -> 4 
  | PLUS  -> 5 
  | MINUS  -> 6 
  | ZERO  -> 7 
  | NL  -> 8 
  | VAR _ -> 9 

// This function maps integer indexes to symbolic token ids
let tokenTagToTokenId (tokenIdx:int) = 
  match tokenIdx with
  | 0 -> TOKEN_ASSIGN 
  | 1 -> TOKEN_SEP 
  | 2 -> TOKEN_LBRA 
  | 3 -> TOKEN_RBRA 
  | 4 -> TOKEN_EOF 
  | 5 -> TOKEN_PLUS 
  | 6 -> TOKEN_MINUS 
  | 7 -> TOKEN_ZERO 
  | 8 -> TOKEN_NL 
  | 9 -> TOKEN_VAR 
  | 12 -> TOKEN_end_of_input
  | 10 -> TOKEN_error
  | _ -> failwith "tokenTagToTokenId: bad token"

/// This function maps production indexes returned in syntax errors to strings representing the non terminal that would be produced by that production
let prodIdxToNonTerminal (prodIdx:int) = 
  match prodIdx with
    | 0 -> NONTERM__startstart 
    | 1 -> NONTERM_start 
    | 2 -> NONTERM_AExpr 
    | 3 -> NONTERM_AExpr 
    | 4 -> NONTERM_AExprA 
    | 5 -> NONTERM_Ass 
    | 6 -> NONTERM_Ass 
    | 7 -> NONTERM_Assa 
    | 8 -> NONTERM_Assa 
    | 9 -> NONTERM_Arr 
    | 10 -> NONTERM_Arr 
    | 11 -> NONTERM_a 
    | 12 -> NONTERM_a 
    | 13 -> NONTERM_a 
    | _ -> failwith "prodIdxToNonTerminal: bad production index"

let _fsyacc_endOfInputTag = 12 
let _fsyacc_tagOfErrorTerminal = 10

// This function gets the name of a token as a string
let token_to_string (t:token) = 
  match t with 
  | ASSIGN  -> "ASSIGN" 
  | SEP  -> "SEP" 
  | LBRA  -> "LBRA" 
  | RBRA  -> "RBRA" 
  | EOF  -> "EOF" 
  | PLUS  -> "PLUS" 
  | MINUS  -> "MINUS" 
  | ZERO  -> "ZERO" 
  | NL  -> "NL" 
  | VAR _ -> "VAR" 

// This function gets the data carried by a token as an object
let _fsyacc_dataOfToken (t:token) = 
  match t with 
  | ASSIGN  -> (null : System.Object) 
  | SEP  -> (null : System.Object) 
  | LBRA  -> (null : System.Object) 
  | RBRA  -> (null : System.Object) 
  | EOF  -> (null : System.Object) 
  | PLUS  -> (null : System.Object) 
  | MINUS  -> (null : System.Object) 
  | ZERO  -> (null : System.Object) 
  | NL  -> (null : System.Object) 
  | VAR _fsyacc_x -> Microsoft.FSharp.Core.Operators.box _fsyacc_x 
let _fsyacc_gotos = [| 0us; 65535us; 1us; 65535us; 0us; 1us; 2us; 65535us; 0us; 2us; 5us; 6us; 2us; 65535us; 0us; 4us; 5us; 4us; 3us; 65535us; 0us; 7us; 5us; 7us; 9us; 10us; 3us; 65535us; 0us; 8us; 5us; 8us; 9us; 8us; 2us; 65535us; 15us; 16us; 18us; 19us; 3us; 65535us; 12us; 13us; 14us; 15us; 17us; 18us; |]
let _fsyacc_sparseGotoTableRowOffsets = [|0us; 1us; 3us; 6us; 9us; 13us; 17us; 20us; |]
let _fsyacc_stateToProdIdxsTableElements = [| 1us; 0us; 1us; 0us; 1us; 1us; 1us; 1us; 2us; 2us; 3us; 1us; 2us; 1us; 2us; 1us; 4us; 2us; 5us; 6us; 1us; 5us; 1us; 5us; 2us; 7us; 8us; 2us; 7us; 8us; 1us; 7us; 1us; 8us; 1us; 8us; 1us; 8us; 1us; 9us; 1us; 9us; 1us; 9us; 1us; 10us; 1us; 11us; 1us; 12us; 1us; 13us; |]
let _fsyacc_stateToProdIdxsTableRowOffsets = [|0us; 2us; 4us; 6us; 8us; 11us; 13us; 15us; 17us; 20us; 22us; 24us; 27us; 30us; 32us; 34us; 36us; 38us; 40us; 42us; 44us; 46us; 48us; 50us; |]
let _fsyacc_action_rows = 24
let _fsyacc_actionTableElements = [|1us; 32768us; 9us; 11us; 0us; 49152us; 1us; 32768us; 4us; 3us; 0us; 16385us; 1us; 16387us; 8us; 5us; 1us; 32768us; 9us; 11us; 0us; 16386us; 0us; 16388us; 1us; 16390us; 1us; 9us; 1us; 32768us; 9us; 11us; 0us; 16389us; 1us; 32768us; 0us; 12us; 4us; 32768us; 2us; 14us; 5us; 21us; 6us; 22us; 7us; 23us; 0us; 16391us; 3us; 32768us; 5us; 21us; 6us; 22us; 7us; 23us; 2us; 32768us; 1us; 17us; 3us; 20us; 0us; 16392us; 3us; 32768us; 5us; 21us; 6us; 22us; 7us; 23us; 2us; 32768us; 1us; 17us; 3us; 20us; 0us; 16393us; 0us; 16394us; 0us; 16395us; 0us; 16396us; 0us; 16397us; |]
let _fsyacc_actionTableRowOffsets = [|0us; 2us; 3us; 5us; 6us; 8us; 10us; 11us; 12us; 14us; 16us; 17us; 19us; 24us; 25us; 29us; 32us; 33us; 37us; 40us; 41us; 42us; 43us; 44us; |]
let _fsyacc_reductionSymbolCounts = [|1us; 2us; 3us; 1us; 1us; 3us; 1us; 3us; 5us; 3us; 1us; 1us; 1us; 1us; |]
let _fsyacc_productionToNonTerminalTable = [|0us; 1us; 2us; 2us; 3us; 4us; 4us; 5us; 5us; 6us; 6us; 7us; 7us; 7us; |]
let _fsyacc_immediateActions = [|65535us; 49152us; 65535us; 16385us; 65535us; 65535us; 16386us; 16388us; 65535us; 65535us; 16389us; 65535us; 65535us; 16391us; 65535us; 65535us; 16392us; 65535us; 65535us; 16393us; 16394us; 16395us; 16396us; 16397us; |]
let _fsyacc_reductions ()  =    [| 
# 139 "AbstractMemoryParser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : AbstractExpression)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
                      raise (FSharp.Text.Parsing.Accept(Microsoft.FSharp.Core.Operators.box _1))
                   )
                 : '_startstart));
# 148 "AbstractMemoryParser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : AbstractExpression)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 25 "AbstractMemoryParser.fsp"
                                                         _1 
                   )
# 25 "AbstractMemoryParser.fsp"
                 : AbstractExpression));
# 159 "AbstractMemoryParser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : AbstractExpression)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : AbstractExpression)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 28 "AbstractMemoryParser.fsp"
                                                        Abs(_1,_3) 
                   )
# 28 "AbstractMemoryParser.fsp"
                 : AbstractExpression));
# 171 "AbstractMemoryParser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : AbstractExpression)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 29 "AbstractMemoryParser.fsp"
                                                         _1 
                   )
# 29 "AbstractMemoryParser.fsp"
                 : AbstractExpression));
# 182 "AbstractMemoryParser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : AbstractAssignment)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 32 "AbstractMemoryParser.fsp"
                                                         AbsE(_1)
                   )
# 32 "AbstractMemoryParser.fsp"
                 : AbstractExpression));
# 193 "AbstractMemoryParser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : AbstractAssignment)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : AbstractAssignment)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 35 "AbstractMemoryParser.fsp"
                                                         AAss(_1,_3) 
                   )
# 35 "AbstractMemoryParser.fsp"
                 : AbstractAssignment));
# 205 "AbstractMemoryParser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : AbstractAssignment)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 36 "AbstractMemoryParser.fsp"
                                                         _1 
                   )
# 36 "AbstractMemoryParser.fsp"
                 : AbstractAssignment));
# 216 "AbstractMemoryParser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : string)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : Sign)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 39 "AbstractMemoryParser.fsp"
                                                         AVar(_1,_3)
                   )
# 39 "AbstractMemoryParser.fsp"
                 : AbstractAssignment));
# 228 "AbstractMemoryParser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : string)) in
            let _4 = (let data = parseState.GetInput(4) in (Microsoft.FSharp.Core.Operators.unbox data : Sign)) in
            let _5 = (let data = parseState.GetInput(5) in (Microsoft.FSharp.Core.Operators.unbox data : AbstractArrayElem)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 40 "AbstractMemoryParser.fsp"
                                                         AArr(_1,AElem(_4,_5)) 
                   )
# 40 "AbstractMemoryParser.fsp"
                 : AbstractAssignment));
# 241 "AbstractMemoryParser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _2 = (let data = parseState.GetInput(2) in (Microsoft.FSharp.Core.Operators.unbox data : Sign)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : AbstractArrayElem)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 43 "AbstractMemoryParser.fsp"
                                                         AElem(_2,_3) 
                   )
# 43 "AbstractMemoryParser.fsp"
                 : AbstractArrayElem));
# 253 "AbstractMemoryParser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 44 "AbstractMemoryParser.fsp"
                                                         AEMP 
                   )
# 44 "AbstractMemoryParser.fsp"
                 : AbstractArrayElem));
# 263 "AbstractMemoryParser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 47 "AbstractMemoryParser.fsp"
                                                         P 
                   )
# 47 "AbstractMemoryParser.fsp"
                 : Sign));
# 273 "AbstractMemoryParser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 48 "AbstractMemoryParser.fsp"
                                                         M 
                   )
# 48 "AbstractMemoryParser.fsp"
                 : Sign));
# 283 "AbstractMemoryParser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 49 "AbstractMemoryParser.fsp"
                                                         Z 
                   )
# 49 "AbstractMemoryParser.fsp"
                 : Sign));
|]
# 294 "AbstractMemoryParser.fs"
let tables () : FSharp.Text.Parsing.Tables<_> = 
  { reductions= _fsyacc_reductions ();
    endOfInputTag = _fsyacc_endOfInputTag;
    tagOfToken = tagOfToken;
    dataOfToken = _fsyacc_dataOfToken; 
    actionTableElements = _fsyacc_actionTableElements;
    actionTableRowOffsets = _fsyacc_actionTableRowOffsets;
    stateToProdIdxsTableElements = _fsyacc_stateToProdIdxsTableElements;
    stateToProdIdxsTableRowOffsets = _fsyacc_stateToProdIdxsTableRowOffsets;
    reductionSymbolCounts = _fsyacc_reductionSymbolCounts;
    immediateActions = _fsyacc_immediateActions;
    gotos = _fsyacc_gotos;
    sparseGotoTableRowOffsets = _fsyacc_sparseGotoTableRowOffsets;
    tagOfErrorTerminal = _fsyacc_tagOfErrorTerminal;
    parseError = (fun (ctxt:FSharp.Text.Parsing.ParseErrorContext<_>) -> 
                              match parse_error_rich with 
                              | Some f -> f ctxt
                              | None -> parse_error ctxt.Message);
    numTerminals = 13;
    productionToNonTerminalTable = _fsyacc_productionToNonTerminalTable  }
let engine lexer lexbuf startState = (tables ()).Interpret(lexer, lexbuf, startState)
let start lexer lexbuf : AbstractExpression =
    Microsoft.FSharp.Core.Operators.unbox ((tables ()).Interpret(lexer, lexbuf, 0))
