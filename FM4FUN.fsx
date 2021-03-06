// This script implements our interactive FM4FUN tool to parse a program

// Open modules
// The following should be the path to the "FsLexYacc.Runtime.dll"
#r "C:/Users/krist/.nuget/packages/fslexyacc.runtime/10.0.0/lib/net46/FsLexYacc.Runtime.dll"

open FSharp.Text.Lexing
open System
#load "FM4FUNAST.fs"
open FM4FUNAST
#load "FM4FUNParser.fs"
open FM4FUNParser
#load "FM4FUNLexer.fs"
open FM4FUNLexer
#load "FM4FUNCompiler.fs"
open FM4FUNCompiler

// Mutally recursive functions to create a string of the parsed program
let rec generateCExp cexp =
    match cexp with
    | Assign (x,y) -> "" + generateVar x + ":=" + generateAExp y
    | Skip -> "skip"
    | C (c1,c2) -> generateCExp c1 + ";\n" + generateCExp c2
    | If gc -> "if " + generateGCExp gc + "\nfi"
    | Do gc -> "do " + generateGCExp gc + "\nod"
and generateVar var =
    match var with
    | Var v -> v
    | Array (x,y) -> x + "[" + generateAExp y + "]"
and generateAExp aexp = 
    match aexp with
    | Num i -> string i
    | V var -> generateVar var
    | Plus (x,y) -> generateAExp x + "+" + generateAExp y 
    | Minus (x,y) -> generateAExp x + "-" + generateAExp y 
    | Mult (x,y) -> generateAExp x + "*" + generateAExp y 
    | Div (x,y) -> generateAExp x + "/" + generateAExp y 
    | UMinus x -> "-" + generateAExp x 
    | Pow (x,y) -> generateAExp x + "^" + generateAExp y
and generateGCExp gcexp =
    match gcexp with
    | Then (b,c) -> generateBExp b + " -> " + generateCExp c
    | GC (gc1,gc2) -> generateGCExp gc1 + "\n[] " + generateGCExp gc2
and generateBExp bexp = 
    match bexp with
    | True -> "true"
    | False -> "false"
    | SAnd (b1,b2) -> "(" + generateBExp b1 + "&&" + generateBExp b2 + ")"
    | SOr (b1,b2) -> "(" + generateBExp b1 + "||" + generateBExp b2 + ")"
    | And (b1,b2) -> "(" + generateBExp b1 + "&" + generateBExp b2 + ")"
    | Or (b1,b2) -> "(" + generateBExp b1 + "|" + generateBExp b2 + ")"
    | Not b -> "!" + "(" + generateBExp b + ")"
    | Eq (a1,a2) -> "(" + generateAExp a1 + "=" + generateAExp a2 + ")"
    | Neq (a1,a2) -> "(" + generateAExp a1 + "!=" + generateAExp a2 + ")"
    | Gt (a1,a2) -> "(" + generateAExp a1 + ">" + generateAExp a2 + ")"
    | Geq (a1,a2) -> "(" + generateAExp a1 + ">=" + generateAExp a2 + ")"
    | Lt (a1,a2) -> "(" + generateAExp a1 + "<" + generateAExp a2 + ")"
    | Leq (a1,a2) -> "(" + generateAExp a1 + "<=" + generateAExp a2 + ")"
 
// After generating the string from cexp then we split it into lines
let generateList (str:string) = List.ofArray(str.Split('\n'))

// Increase in indentation happens only when we see "->".
// If the program is parsed correctly, the mutally recursive functions generateCExp ... generateBExp
// will ensure that, when we split the string created by these functions at every "\n", each element
// in the list will contain at most one "->".
// If we then further split this string element into two (e.g. "if true -> x:=a"), the indentation 
// for the lines following the "->" must be the length of the first element (in this case "if true ") 
// plus three (the number of characters in "-> "). Should the element contain no "->", then no further 
// indentation is nedded. getIndentation calculates this.

let getIndentation (str:string) =
    match List.ofArray(str.Split("->")) with
    | x::xs when List.length xs = 1 -> String.length x + 3
    | _ -> 0

// The sum of integers in an int list
let sum list = List.fold (+) 0 list

// Parameter "ind" in addIndentation is an int list containing indentation levels
// Total indentation is found as a sum of elements in the list

let rec addIndentation ind (list:string list) =
    match list with
    // Reduce indentation and add new
    | x::xs when x.Contains("[]") -> 
        let _ :: ys = ind
        let newInd = (getIndentation x) :: ys
        String.replicate (sum ys) " " + x + "\n" + addIndentation newInd xs
    // Reduce indentation
    | x::xs when x.Contains("od") || x.Contains("fi") -> 
        let _ :: ys = ind
        String.replicate (sum ys) " " + x + "\n" + addIndentation ys xs
    // Keep indentation
    | x::xs -> 
        let temp = getIndentation x 
        String.replicate (sum ind) " " + x + "\n" + addIndentation (if temp = 0 then ind else temp :: ind) xs
    | [] -> ""

// Prettify the abstract syntax tree
let prettify cexp =
    cexp
    |> generateCExp
    |> generateList
    |> addIndentation []
    
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////// THIS IS FOR TASK 2 ///////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

// Generate edge label 
let actToString act = 
    match act with
    | A a -> generateCExp a
    | B b -> generateBExp b
    | S s -> generateCExp s

// Generate graphviz edge string
let edgeToString (E((N startStr), act, (N endStr))) = startStr + " -> " + endStr + " [label = \"" + actToString act + "\"];"   

// Collect all graphviz edge strings
let edgesToString elist = List.fold (fun a e -> a + edgeToString e + "\n") "" elist

// Generate graphviz code from PG
let prettifyPG (_, _, _, elist) = 
    let prefix = "digraph program_graph {rankdir=LR; \nnode [shape = circle]\n"
    let postfix = "}"
    let edges = edgesToString elist
    prefix + edges + postfix

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////  
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////  

// Method below allows for multiple-line input from the user
// Press enter twice to finish input
let rec getInput (str:string) = 
    let input = Console.ReadLine()
    match input with
    | "" -> str.Substring(0,str.Length-1)
    | _ -> getInput (str + input + "\n")


// We implement here the function that interacts with the user with n tries
let rec compute n =
    if n = 0 then
        printfn "Bye bye"
    else
        printfn "\nEnter an GCL-command\nThe command cannot have two consecutive newlines\n(Press enter twice to finish input):"
        let input = getInput ""
        let lexbuf = LexBuffer<char>.FromString input
        try
            // We parse the input string
            let res = FM4FUNParser.start FM4FUNLexer.tokenize lexbuf
            printfn "############### Parsing successful! ############### \n%s" (prettify res) 
            printfn "Do you want to create a program graph? \nDet / NonDet / No"
            match Console.ReadLine() with 
                | "Det" -> let pg = FM4FUNCompiler.constructPG res Det
                           printfn "\n############### F# type Program Graph! ############### \n%A" pg
                           printfn "\n############### Graphviz version! ############### \n%s\n" (prettifyPG pg)    
                | "NonDet" -> let pg = FM4FUNCompiler.constructPG res NonDet
                              printfn "\n############### F# type Program Graph! ############### \n%A" pg
                              printfn "\n############### Graphviz version! ############### \n%s\n" (prettifyPG pg)                 
                | _ -> ()
            
            // Get ready for a new input
            compute n

        with err -> 
            // In case the program is not accepted, some hints are printed
            // indicating where the error occured
            let endPos = lexbuf.EndPos
            let linePos = endPos.Line
            let colPos = endPos.Column
            let lexString = LexBuffer<char>.LexemeString(lexbuf)
            printfn "#### Error around: %A line %d col %d\n" lexString linePos colPos
            // Get ready for a new input
            compute (n-1)

// Start interacting with the user
compute 10
