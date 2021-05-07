// This script implements our interactive FM4FUN tool to parse a program

// Open modules
// The following should be the path to the "FsLexYacc.Runtime.dll"
#r "C:/Users/Jahar/.nuget/packages/fslexyacc.runtime/10.0.0/lib/net46/FsLexYacc.Runtime.dll"

open FSharp.Text.Lexing
open System

#load "FM4FUN/FM4FUNAST.fs"
open FM4FUNAST
#load "FM4FUN/FM4FUNParser.fs"
open FM4FUNParser
#load "FM4FUN/FM4FUNLexer.fs"
open FM4FUNLexer

#load "FM4FUN/FM4FUNCompiler.fs"
open FM4FUNCompiler
#load "FM4FUN/FM4FUNInterpreter.fs"
open FM4FUNInterpreter

#load "ConcreteMemory/ConcreteMemoryAST.fs"
open ConcreteMemoryAST
#load "ConcreteMemory/ConcreteMemoryParser.fs"
open ConcreteMemoryParser
#load "ConcreteMemory/ConcreteMemoryLexer.fs"
open ConcreteMemoryLexer

#load "SignAnalysis/AbstractMemoryAST.fs"
open AbstractMemoryAST
#load "SignAnalysis/AbstractMemoryParser.fs"
open AbstractMemoryParser
#load "SignAnalysis/AbstractMemoryLexer.fs"
open AbstractMemoryLexer

#load "SignAnalysis/SignAnalysis.fs"
open SignAnalysis

#load "SecurityAnalysis/ClassificationAST.fs"
open ClassificationAST
#load "SecurityAnalysis/ClassificationParser.fs"
open ClassificationParser
#load "SecurityAnalysis/ClassificationLexer.fs"
open ClassificationLexer

#load "SecurityAnalysis/LatticeAST.fs"
open LatticeAST
#load "SecurityAnalysis/LatticeParser.fs"
open LatticeParser
#load "SecurityAnalysis/LatticeLexer.fs"
open LatticeLexer

#load "SecurityAnalysis/SecurityAnalysis.fs"
open SecurityAnalysis

#load "ModelChecker/ModelChecker.fs"
open ModelChecker

type Status = Terminated | Stuck

exception MemoryNotWellDefined of string

// Mutally recursive functions to create a string of the parsed program
let rec generateCExp cexp =
    match cexp with
    | Assign (x,y) -> "" + generateVar x + ":=" + generateAExp y
    | Skip -> "skip"
    | Break -> "break"
    | Continue -> "continue"
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
    | Mod (x,y) -> generateAExp x + "%" + generateAExp y 
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
    | Geq (a1,a2) ->  "(" + generateAExp a1 + ">=" + generateAExp a2 + ")"
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
    | S -> "skip"

// Generate graphviz edge string
let edgeToString ((N startStr), act, (N endStr)) = startStr + " -> " + endStr + " [label = \"" + actToString act + "\"];"   

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

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////// THIS IS FOR TASK 3 ///////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

let sortNodeList (nodeList: Node list) = //take list of nodes, sort them (qstart, q1..qi, qend), return list of nodes. 
    let avgNodes = List.filter (fun (N s) -> s <> "qstart" && s <> "qend" ) nodeList 
    N "qstart" :: (List.sort avgNodes) @ [N "qend"]

let updateMemory v l mem = Map.empty

let rec retrieveArray a =
    match a with
    | Elem(i,ae) -> i::retrieveArray ae
    | EMP -> []

let rec initializeArray v i arr mem =
    match arr with
    | x::xs -> initializeArray v (i+1) xs (Map.add (v,i) x mem)
    | [] -> mem

let rec initializeMemory res mem =
    let (varMem,arrMem) = mem
    match res with
    | UAss (a1,a2) -> initializeMemory a1 (initializeMemory a2 mem)
    | UVar (v,i) -> (Map.add v i varMem,arrMem)
    | UArr (v,ae) -> (varMem, initializeArray v 0 (retrieveArray ae) arrMem)

let rec retrieveAbstractArray a =
    match a with
    | AElem(i,ae) -> i::retrieveAbstractArray ae
    | AEMP -> []

let rec initializeAbstractArray v arr mem =
    match arr with
    | x::xs -> initializeAbstractArray v xs (addToMap v x mem)
    | [] -> mem

let rec initializeAbstractMemory res mem =
    let (varMem,arrMem) = mem
    match res with
    | AAss (a1,a2) -> initializeAbstractMemory a1 (initializeAbstractMemory a2 mem)
    | AVar (v,i) -> (Map.add v i varMem,arrMem)
    | AArr (v,ae) -> (varMem, initializeAbstractArray v (retrieveAbstractArray ae) arrMem)

let rec initializeAbstractMemories res mems =
    match res with
    | Abs (a1, a2) -> initializeAbstractMemories a2 (initializeAbstractMemories a1 mems)
    | AbsE (a) -> Set.add (initializeAbstractMemory a (Map.ofList [], Map.ofList [])) mems

let isWellDefinedMemory variables mem = 
    let (varMem,arrMem) = mem
    List.forall (fun x -> Map.containsKey x varMem || Map.containsKey (x,0) arrMem) variables

let isWellDefinedAbstractMemory variables mem = 
    let (varMem,arrMem) = mem
    List.forall (fun x -> Map.containsKey x varMem || Map.containsKey (x) arrMem) variables    

let isWellDefinedAbstractMemories variables mems = Set.forall (fun e -> isWellDefinedAbstractMemory variables e) mems

let defineTag input = 
    match input with
    | "det" -> Det
    | "nondet" -> NonDet
    | _ -> Undef

let findOutgoingEdges elist node = List.filter (fun (qstart,_,_) -> node = qstart) elist

let rec findNext elist mem = 
    if elist = [] then failwith "Stuck" 
    else
        let (_,act,qend)::es = elist
        try
            (semantics act mem, qend)
        with err -> 
            findNext es mem

let executePG (_,(qstart,qend),_,elist) mem = 
    let rec execute node mem =
        let outgoingEdges = findOutgoingEdges elist node
        try
            let (newMem, qnext) = findNext outgoingEdges mem
            execute qnext newMem
        with err ->
            if node = qend then (Terminated, node, mem) else (Stuck, node, mem)
    execute qstart mem

let prettifyMemory (varMem,arrMem) =
    let varString = List.fold (fun acc (v,a) -> acc+v+": "+string a+"\n") "" (Map.toList varMem)
    varString + List.fold (fun acc ((v,i),a) -> acc+v+"["+string i+"]: "+string a+"\n") "" (Map.toList arrMem)

let prettifyEndState (s,N(q),mem) = "Status: "+s.ToString()+"\nNode: "+q+"\n"+prettifyMemory mem

let prettifyAbstractMemories mems = Set.fold (fun a e -> a + (prettifyMemory e)) "" mems

let signToString v = 
    match v with
    | P -> "+"
    | M -> "-"
    | Z -> "0"

let prettifySingleSolution (varMem, arrMem) = 
    let varString = Map.fold (fun acc k v -> acc + (signToString v) + "\t") "" varMem
    (Map.fold (fun acc k v -> let arrString = Set.fold (fun a e -> a + signToString e + ", ") "" v
                              acc + "{ " + (arrString.Substring(0, arrString.Length-2))  + " }" + "\t"
                              ) varString arrMem)
                                              
// Node    i   n   x   y   A
// qs      +   +   +   +   {+}
//         +   -   +   +   {+,0}
// q1      +   +   +   +   {+}

let prettifyAnalysisSolution sol nodeList variables = //nodeList should already be "sorted"
    let (varMem, arrMem) = Set.minElement (Map.find (N "qstart") sol)
    let vars = Map.foldBack (fun k _ acc -> k::acc) varMem (Map.foldBack (fun k _ acc-> k::acc) arrMem [])
    let variableList = List.fold (fun acc e -> acc + e + "\t") "Nodes\t" vars
    List.fold (fun acc (N s) -> let v = Map.find (N s) sol
                                acc + s + 
                                if Set.count(v) <> 0 
                                    then Set.fold (fun a e -> a + "\t" + prettifySingleSolution e + "\n")"" v 
                                    else "\n" 
              ) (variableList + "\n") nodeList           

let prettifyFRs frs = let (fullStr:string) = Set.fold (fun str (x,y) -> str + x + " -> " + y+", ") "" frs
                      if fullStr.Length = 0 then "" else fullStr.Substring(0,fullStr.Length-2)

let rec addStatus stuckStates = 
    match stuckStates with
    | [] -> []
    | (N s, mem)::res when s = "qend" -> (Terminated, N s, mem)::(addStatus res)
    | (N s, mem)::res -> (Stuck, N s, mem)::(addStatus res) 

let prettifyEndStates states = List.fold (fun acc elem -> acc + prettifyEndState elem + "\n") "" states                            

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////  
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////  

let stringFromLexBuffer (lexbuf:LexBuffer<char>) = 
    let endPos = lexbuf.EndPos
    let linePos = endPos.Line
    let colPos = endPos.Column
    let lexString = LexBuffer<char>.LexemeString(lexbuf)
    sprintf "#### Error around: %A line %d col %d\n" lexString linePos colPos
// We implement here the function that interacts with the user with n tries
let rec compute n =
    if n = 0 then
        printfn "Bye bye"
    else
        printfn "\nEnter an GCL-command\nThe command cannot have two consecutive newlines\n(Press enter twice to finish input):"
        let input = getInput ""
        let lexbuf = LexBuffer<char>.FromString input
        
        try
            // Parsing the GCL-command
            let res = FM4FUNParser.start FM4FUNLexer.tokenize lexbuf

            printfn "what do you want, biitch.\n 1: Step-wise execution \n 2: Detection of signs analysis \n 3: Security analysis \n 4: Model Checker"
            let answer = Console.ReadLine()
            match answer with
            | "1" -> // Step-wise execution
                     // Get initial values from input
                     printf "\nEnter initial values for all variables in your program:\n"
                     let initialValues = Console.ReadLine()
                     let lexbufInput = LexBuffer<char>.FromString initialValues
                     try
                         // Create memory from initial values string (Step-Wise Execution)
                         let resInput = ConcreteMemoryParser.start ConcreteMemoryLexer.tokenize lexbufInput
                         let mem = initializeMemory resInput (Map.ofList [],Map.ofList[])
                         printfn "Initialized memory: %s" (prettifyMemory mem)

                         // Create program graph (Step-Wise Execution)
                         printfn "\nDo you want to execute the program graph? \nDet / NonDet / No\n(For execution only deterministic version is implemented)\n"
                         let tag = defineTag ((Console.ReadLine()).ToLower())
                         let pg = FM4FUNCompiler.constructPG res tag

                         // Check if all variables in program have initial values (Step-Wise Execution)
                         let variables = findVariables pg
                         printfn "Variables: %A" variables

                         if not (isWellDefinedMemory (Set.toList variables) mem) then raise (MemoryNotWellDefined "Memory not well defined")

                         printfn "\n############### Parsing successful! ############### \n%s" (prettify res)

                         printf "Execution:\n%s\n" (prettifyEndState (executePG pg mem))

                         printfn "\n############### Graphviz version! ############### \n%s\n" (prettifyPG pg)
                     with
                        // Handles the error raised if memory is not well-defined
                        | MemoryNotWellDefined e -> printfn "Error: %s" e
                        // handles other errors, oops.
                        | Failure e -> printfn "Error: %A" e
                        // Handles the error from parsing the initial values
                        | err -> printfn "%s" (stringFromLexBuffer lexbufInput)
                                 compute (n-1)

            | "2" -> // Detection of sign analysis
                     // Get initial abstract values from input
                     printf "Enter initial abstract values for all variables in your program (hit enter twice to end input and once for new memory):\n"
                     let initialValues = getInput ""
                     let lexbufInput = LexBuffer<char>.FromString initialValues
                     try
                         // Create Abstract memory from initial values string (Step-Wise Execution)
                         let resInput = AbstractMemoryParser.start AbstractMemoryLexer.tokenize lexbufInput
                         let mems = initializeAbstractMemories resInput (Set.ofList [])

                         let pg = FM4FUNCompiler.constructPG res Det
                         let (states,_,_,_) = pg

                         // Check if all variables in program have initial values (Step-Wise Execution)
                         let variables = findVariables pg

                         if not (isWellDefinedAbstractMemories (Set.toList variables) mems) then raise (MemoryNotWellDefined "Memory not well defined")

                         let sol = (computeSolution pg mems)

                         Map.iter (fun k v -> printfn "%A" v ) sol

                         printfn "\n############### Parsing successful! ############### \n%s" (prettifyAnalysisSolution sol (sortNodeList states) variables)

                     with
                        // Handles the error raised if memory is not well-defined
                        | MemoryNotWellDefined e -> printfn "Error: %s" e
                        // Handles the error from parsing the initial values
                        | err -> printfn "Error: %A" err
                                 printfn "%s" (stringFromLexBuffer lexbufInput)
                                 compute (n-1)

            | "3" -> // Security analysis
                     let variables = Set.ofList (findVarCexp res)
                     // Get security lattice from input
                     printf "Enter security lattice:\n"
                     let securityLatticeInput = Console.ReadLine()
                     let lexbufSecurityLatticeInput = LexBuffer<char>.FromString securityLatticeInput
                     
                     try
                        // Create Abstract memory from initial values string (Step-Wise Execution)
                        let securityLattice = LatticeParser.start LatticeLexer.tokenize lexbufSecurityLatticeInput

                        // Compute lattice
                        let FL = computeFL securityLattice

                        printf "Enter security classification:\n"
                        let classificationInput = Console.ReadLine()
                        let lexbufClassificationInput = LexBuffer<char>.FromString classificationInput
                        try
                        // Create Abstract memory from initial values string (Step-Wise Execution)
                        let classification = ClassificationParser.start ClassificationLexer.tokenize lexbufClassificationInput
                        // Compute security classification
                        let L = computeSC classification (Map.ofList [])
                        // Does all variables have a entry in security classification
                        if not (isWellDefinedSC variables L) then failwith "Security Classification is not well defined."


                        let actualFR = sec res (Set.ofList [])

                        let allowedFR = findAllowedFlowRelations variables L FL

                        let violatingFR = Set.difference actualFR allowedFR

                        let result = if (Set.count violatingFR) = 0 then "secure" else "not secure"

                        printfn "\nActual: %s" (prettifyFRs actualFR)
                        printfn "Allowed: %s" (prettifyFRs allowedFR)
                        printfn "Violations: %s" (prettifyFRs violatingFR)
                        printfn "Result: %s" result

                        with
                        // Handles the error from parsing the initial values
                        | err -> printfn "Error: %s" err.Message
                                 printfn "%s" (stringFromLexBuffer lexbufClassificationInput)
                                 compute (n-1)
                     with
                        // Handles the error from parsing the initial values
                        | err -> printfn "Error: %s" err.Message
                                 printfn "%s" (stringFromLexBuffer lexbufSecurityLatticeInput)
                                 compute (n-1)
            | "4" -> // Get initial values from input
                     printf "\nEnter initial values for all variables in your program:\n"
                     let initialValues = Console.ReadLine()
                     let lexbufInput = LexBuffer<char>.FromString initialValues
                     try
                         // Create memory from initial values string (Step-Wise Execution)
                         let resInput = ConcreteMemoryParser.start ConcreteMemoryLexer.tokenize lexbufInput
                         let mem = initializeMemory resInput (Map.ofList [],Map.ofList[])

                         // Create program graph (Step-Wise Execution)
                         printfn "\nDo you want to execute the program graph? \nDet / NonDet / No\n"
                         let tag = defineTag ((Console.ReadLine()).ToLower())
                         let pg = FM4FUNCompiler.constructPG res tag

                         // Check if all variables in program have initial values (Step-Wise Execution)
                         let variables = findVariables pg

                         if not (isWellDefinedMemory (Set.toList variables) mem) then raise (MemoryNotWellDefined "Memory not well defined")

                         //check stuck configs
                         let (nodeList, (qstart, qend), acts, edges) = pg
                         let initialConfig = (qstart, mem)
                         let stuckStates = stuckConfigurationCheckerTest edges (Set.ofList [initialConfig]) 100
                         let stuckStatesWithStatus = addStatus (Set.toList stuckStates)

                         printf "Stuck states: \n%s\n" (prettifyEndStates stuckStatesWithStatus)

                     with
                        // Handles the error raised if memory is not well-defined
                        | MemoryNotWellDefined e -> printfn "Error: %s" e
                        // handles other errors, oops.
                        | Failure e -> printfn "Error: %A" e
                        // Handles the error from parsing the initial values
                        | err -> printfn "%s" (stringFromLexBuffer lexbufInput)
                                 compute (n-1)

            | _ -> compute n
            // Get ready for a new input
            compute n
        with 
            // Handles the error from parsing the GCL-program
            | err -> printfn "%s" (stringFromLexBuffer lexbuf)
                     compute (n-1)
               

// Start interacting with the user with 10 tries
compute 10
