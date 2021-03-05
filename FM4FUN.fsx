// This script implements our interactive calculator

// We need to import a couple of modules, including the generated lexer and parser
#r "C:/Users/Jahar/.nuget/packages/fslexyacc.runtime/10.0.0/lib/net46/FsLexYacc.Runtime.dll"
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
let generateLines (cexp:cexp) =
    match cexp with
    | _ -> [""]

let addIndentation strs =
    match strs with
    | x::xs -> [""]
    | _ -> [""]

let prettify (cexp:cexp) = 
    let rec combine l s = 
        match l with
        | ind::str::ls -> combine ls (s+ind+str+"\n")
        | _ -> s
    combine (addIndentation (generateLines cexp)) ""
// We
let parse input =
    // translate string into a buffer of characters
    let lexbuf = LexBuffer<char>.FromString input
    // translate the buffer into a stream of tokens and parse them
    let res = FM4FUNParser.start FM4FUNLexer.tokenize lexbuf
    // return the result of parsing (i.e. value of type "expr")
    res

// We implement here the function that interacts with the user
let rec compute n =
    if n = 0 then
        printfn "Bye bye"
    else
        try
            printf "Enter an GCL-command: "

            // We parse the input string
            let e = parse (Console.ReadLine())
            printfn "%A" e
            compute n
        with err -> 
            printfn "%s" (err.ToString())
            compute (n-1)

// Start interacting with the user
compute 1000

