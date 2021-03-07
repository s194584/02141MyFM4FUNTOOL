// This script implements our interactive calculator

// We need to import a couple of modules, including the generated lexer and parser
#r "C:/Users/Bruger/.nuget/packages/fslexyacc.runtime/10.0.0/lib/net46/FsLexYacc.Runtime.dll"
open FSharp.Text.Lexing
open System
#load "FM4FUNAST.fs"
open FM4FUNAST
#load "FM4FUNParser.fs"
open FM4FUNParser
#load "FM4FUNLexer.fs"
open FM4FUNLexer

// We only have to indent when we enter do or if
// the indentation will be the length from the keyword
// to the THEN (->) + 1.
// The end keywords would then reduce the indentation again.
//      The above could be done with recursion?
// So we can maybe generate the lines in order. (Array of strings)
// Then go through calculating the length of the 
// needed indentation. (Insert space (' ') strings into the correct )
// Lastly generate the strings and add 'newlines'

// Line change: 1) with every ";" 2) end of "if" and "do" 3) before every "[]"

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
    | SAnd (b1,b2) -> generateBExp b1 + "&&" + generateBExp b2
    | SOr (b1,b2) -> generateBExp b1 + "||" + generateBExp b2
    | And (b1,b2) -> generateBExp b1 + "&" + generateBExp b2
    | Or (b1,b2) -> generateBExp b1 + "|" + generateBExp b2
    | Not b -> "!" + generateBExp b
    | Eq (a1,a2) -> generateAExp a1 + "=" + generateAExp a2
    | Neq (a1,a2) -> generateAExp a1 + "!=" + generateAExp a2
    | Gt (a1,a2) -> generateAExp a1 + ">" + generateAExp a2
    | Geq (a1,a2) -> generateAExp a1 + ">=" + generateAExp a2
    | Lt (a1,a2) -> generateAExp a1 + "<" + generateAExp a2
    | Leq (a1,a2) -> generateAExp a1 + "<=" + generateAExp a2
 
let generateList (str:string) = List.ofArray(str.Split('\n'))

let getIndentation (str:string) = 
    let x::xs = List.ofArray(str.Split("->"))
    if List.length xs = 1 then String.length x + 3 else 0

let sum list = List.fold (+) 0 list

let rec addIndentation (list:string list) ind =
    match list with
    | x::xs when x.Contains("od") || x.Contains("fi") -> 
        let _ :: temp = ind
        String.replicate (sum temp) " " + x + "\n" + addIndentation (xs) temp
    | x::xs -> 
        let temp = getIndentation x 
        String.replicate (sum ind) " " + x + "\n" + addIndentation (xs) (if temp = 0 then ind else temp :: ind)
    | [] -> ""


let prettify (cexp:cexp) = addIndentation (generateList (generateCExp cexp)) [0]

// Method below allows for multiple line input from the user
// Press enter twice to finish input 
let rec getInput str = 
    let input = Console.ReadLine()
    match input with
    | "" -> str
    | _ -> getInput (str + input + " ")


// We implement here the function that interacts with the user
let rec compute n =
    if n = 0 then
        printfn "Bye bye"
    else
        printfn "Enter an GCL-command: (Press enter twice to finish input)"
        let input = getInput ""
        let lexbuf = LexBuffer<char>.FromString input

        try
            // We parse the input string
            let res = FM4FUNParser.start FM4FUNLexer.tokenize lexbuf
            printfn "Compile succes! \n%s" (prettify res)
            compute n

        with err -> 
            // In case the program is not accepted, some hint are printed
            // indicating where the error occured
            let endPos = lexbuf.EndPos
            let colPos = endPos.Column
            let lexString = LexBuffer<char>.LexemeString(lexbuf)
            printfn "Parse error at: %A position %A" lexString colPos
            compute (n-1)

// Start interacting with the user
compute 1000
