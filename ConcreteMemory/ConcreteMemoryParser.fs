// Implementation file for parser generated by fsyacc
module ConcreteMemoryParser
#nowarn "64";; // turn off warnings that type variables used in production annotations are instantiated to concrete type
open FSharp.Text.Lexing
open FSharp.Text.Parsing.ParseHelpers
# 2 "ConcreteMemoryParser.fsp"

open ConcreteMemoryAST

# 10 "ConcreteMemoryParser.fs"
// This type is the type of tokens accepted by the parser
type token = 
  | ASSIGN
  | SEP
  | LBRA
  | RBRA
  | EOF
  | VAR of (string)
  | NUM of (int)
// This type is used to give symbolic names to token indexes, useful for error messages
type tokenId = 
    | TOKEN_ASSIGN
    | TOKEN_SEP
    | TOKEN_LBRA
    | TOKEN_RBRA
    | TOKEN_EOF
    | TOKEN_VAR
    | TOKEN_NUM
    | TOKEN_end_of_input
    | TOKEN_error
// This type is used to give symbolic names to token indexes, useful for error messages
type nonTerminalId = 
    | NONTERM__startstart
    | NONTERM_start
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
  | VAR _ -> 5 
  | NUM _ -> 6 

// This function maps integer indexes to symbolic token ids
let tokenTagToTokenId (tokenIdx:int) = 
  match tokenIdx with
  | 0 -> TOKEN_ASSIGN 
  | 1 -> TOKEN_SEP 
  | 2 -> TOKEN_LBRA 
  | 3 -> TOKEN_RBRA 
  | 4 -> TOKEN_EOF 
  | 5 -> TOKEN_VAR 
  | 6 -> TOKEN_NUM 
  | 9 -> TOKEN_end_of_input
  | 7 -> TOKEN_error
  | _ -> failwith "tokenTagToTokenId: bad token"

/// This function maps production indexes returned in syntax errors to strings representing the non terminal that would be produced by that production
let prodIdxToNonTerminal (prodIdx:int) = 
  match prodIdx with
    | 0 -> NONTERM__startstart 
    | 1 -> NONTERM_start 
    | 2 -> NONTERM_Ass 
    | 3 -> NONTERM_Ass 
    | 4 -> NONTERM_Assa 
    | 5 -> NONTERM_Assa 
    | 6 -> NONTERM_Arr 
    | 7 -> NONTERM_Arr 
    | 8 -> NONTERM_a 
    | _ -> failwith "prodIdxToNonTerminal: bad production index"

let _fsyacc_endOfInputTag = 9 
let _fsyacc_tagOfErrorTerminal = 7

// This function gets the name of a token as a string
let token_to_string (t:token) = 
  match t with 
  | ASSIGN  -> "ASSIGN" 
  | SEP  -> "SEP" 
  | LBRA  -> "LBRA" 
  | RBRA  -> "RBRA" 
  | EOF  -> "EOF" 
  | VAR _ -> "VAR" 
  | NUM _ -> "NUM" 

// This function gets the data carried by a token as an object
let _fsyacc_dataOfToken (t:token) = 
  match t with 
  | ASSIGN  -> (null : System.Object) 
  | SEP  -> (null : System.Object) 
  | LBRA  -> (null : System.Object) 
  | RBRA  -> (null : System.Object) 
  | EOF  -> (null : System.Object) 
  | VAR _fsyacc_x -> Microsoft.FSharp.Core.Operators.box _fsyacc_x 
  | NUM _fsyacc_x -> Microsoft.FSharp.Core.Operators.box _fsyacc_x 
let _fsyacc_gotos = [| 0us; 65535us; 1us; 65535us; 0us; 1us; 2us; 65535us; 0us; 2us; 5us; 6us; 2us; 65535us; 0us; 4us; 5us; 4us; 2us; 65535us; 11us; 12us; 14us; 15us; 1us; 65535us; 8us; 9us; |]
let _fsyacc_sparseGotoTableRowOffsets = [|0us; 1us; 3us; 6us; 9us; 12us; |]
let _fsyacc_stateToProdIdxsTableElements = [| 1us; 0us; 1us; 0us; 1us; 1us; 1us; 1us; 2us; 2us; 3us; 1us; 2us; 1us; 2us; 2us; 4us; 5us; 2us; 4us; 5us; 1us; 4us; 1us; 5us; 1us; 5us; 1us; 5us; 1us; 6us; 1us; 6us; 1us; 6us; 1us; 7us; 1us; 8us; |]
let _fsyacc_stateToProdIdxsTableRowOffsets = [|0us; 2us; 4us; 6us; 8us; 11us; 13us; 15us; 18us; 21us; 23us; 25us; 27us; 29us; 31us; 33us; 35us; 37us; |]
let _fsyacc_action_rows = 18
let _fsyacc_actionTableElements = [|1us; 32768us; 5us; 7us; 0us; 49152us; 1us; 32768us; 4us; 3us; 0us; 16385us; 1us; 16387us; 1us; 5us; 1us; 32768us; 5us; 7us; 0us; 16386us; 1us; 32768us; 0us; 8us; 2us; 32768us; 2us; 10us; 6us; 17us; 0us; 16388us; 1us; 32768us; 6us; 11us; 2us; 32768us; 1us; 13us; 3us; 16us; 0us; 16389us; 1us; 32768us; 6us; 14us; 2us; 32768us; 1us; 13us; 3us; 16us; 0us; 16390us; 0us; 16391us; 0us; 16392us; |]
let _fsyacc_actionTableRowOffsets = [|0us; 2us; 3us; 5us; 6us; 8us; 10us; 11us; 13us; 16us; 17us; 19us; 22us; 23us; 25us; 28us; 29us; 30us; |]
let _fsyacc_reductionSymbolCounts = [|1us; 2us; 3us; 1us; 3us; 5us; 3us; 1us; 1us; |]
let _fsyacc_productionToNonTerminalTable = [|0us; 1us; 2us; 2us; 3us; 3us; 4us; 4us; 5us; |]
let _fsyacc_immediateActions = [|65535us; 49152us; 65535us; 16385us; 65535us; 65535us; 16386us; 65535us; 65535us; 16388us; 65535us; 65535us; 16389us; 65535us; 65535us; 16390us; 16391us; 16392us; |]
let _fsyacc_reductions ()  =    [| 
# 114 "ConcreteMemoryParser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : UserAssignment)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
                      raise (FSharp.Text.Parsing.Accept(Microsoft.FSharp.Core.Operators.box _1))
                   )
                 : '_startstart));
# 123 "ConcreteMemoryParser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : UserAssignment)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 24 "ConcreteMemoryParser.fsp"
                                                         _1 
                   )
# 24 "ConcreteMemoryParser.fsp"
                 : UserAssignment));
# 134 "ConcreteMemoryParser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : UserAssignment)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : UserAssignment)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 27 "ConcreteMemoryParser.fsp"
                                                         UAss(_1,_3) 
                   )
# 27 "ConcreteMemoryParser.fsp"
                 : UserAssignment));
# 146 "ConcreteMemoryParser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : UserAssignment)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 28 "ConcreteMemoryParser.fsp"
                                                         _1 
                   )
# 28 "ConcreteMemoryParser.fsp"
                 : UserAssignment));
# 157 "ConcreteMemoryParser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : string)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : int)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 31 "ConcreteMemoryParser.fsp"
                                                         UVar(_1,_3)
                   )
# 31 "ConcreteMemoryParser.fsp"
                 : UserAssignment));
# 169 "ConcreteMemoryParser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : string)) in
            let _4 = (let data = parseState.GetInput(4) in (Microsoft.FSharp.Core.Operators.unbox data : int)) in
            let _5 = (let data = parseState.GetInput(5) in (Microsoft.FSharp.Core.Operators.unbox data : ArrayElem)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 32 "ConcreteMemoryParser.fsp"
                                                         UArr(_1,Elem(_4,_5)) 
                   )
# 32 "ConcreteMemoryParser.fsp"
                 : UserAssignment));
# 182 "ConcreteMemoryParser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _2 = (let data = parseState.GetInput(2) in (Microsoft.FSharp.Core.Operators.unbox data : int)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : ArrayElem)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 35 "ConcreteMemoryParser.fsp"
                                                         Elem(_2,_3) 
                   )
# 35 "ConcreteMemoryParser.fsp"
                 : ArrayElem));
# 194 "ConcreteMemoryParser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 36 "ConcreteMemoryParser.fsp"
                                                         EMP 
                   )
# 36 "ConcreteMemoryParser.fsp"
                 : ArrayElem));
# 204 "ConcreteMemoryParser.fs"
        (fun (parseState : FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : int)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 39 "ConcreteMemoryParser.fsp"
                                                         _1 
                   )
# 39 "ConcreteMemoryParser.fsp"
                 : int));
|]
# 216 "ConcreteMemoryParser.fs"
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
    numTerminals = 10;
    productionToNonTerminalTable = _fsyacc_productionToNonTerminalTable  }
let engine lexer lexbuf startState = (tables ()).Interpret(lexer, lexbuf, startState)
let start lexer lexbuf : UserAssignment =
    Microsoft.FSharp.Core.Operators.unbox ((tables ()).Interpret(lexer, lexbuf, 0))