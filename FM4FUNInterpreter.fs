module FM4FUNInterpreter
open FM4FUNAST

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////// THIS IS FOR TASK 3 ///////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

// Helper functions to find all variables in input program
let rec findVarAexp aexp = 
    match aexp with
    | Num n -> []
    | V v -> findVarVexp v
    | UMinus x -> findVarAexp x
    | Plus (x,y) -> findVarAexp x @ findVarAexp y
    | Minus (x,y) -> findVarAexp x @ findVarAexp y
    | Div (x,y) -> findVarAexp x @ findVarAexp y
    | Mult (x,y) -> findVarAexp x @ findVarAexp y
    | Pow (x,y) -> findVarAexp x @ findVarAexp y    
and findVarVexp var = 
    match var with
    | Var v -> [v]
    | Array (a,_) -> [a]
and findVarBexp bexp =
    match bexp with
    | True | False -> []
    | SAnd (b1, b2) -> findVarBexp b1 @ findVarBexp b2
    | SOr (b1, b2) -> findVarBexp b1 @ findVarBexp b2
    | And (b1, b2) -> findVarBexp b1 @ findVarBexp b2
    | Or (b1, b2) -> findVarBexp b1 @ findVarBexp b2 
    | Not b -> findVarBexp b
    | Eq (a1, a2) -> findVarAexp a1 @ findVarAexp a2
    | Neq (a1, a2) -> findVarAexp a1 @ findVarAexp a2
    | Gt (a1, a2) -> findVarAexp a1 @ findVarAexp a2
    | Geq (a1, a2) -> findVarAexp a1 @ findVarAexp a2
    | Lt (a1, a2) -> findVarAexp a1 @ findVarAexp a2
    | Leq (a1, a2) -> findVarAexp a1 @ findVarAexp a2
and findVarCexp cexp = 
    match cexp with
    | Assign (v,a) -> findVarVexp v @ findVarAexp a
    | Skip -> []
    | _ -> failwith "Not supposed to happen"

let findVar (qs, act, qe) =
    match act with
    | B b -> findVarBexp b 
    | S -> []
    | A a -> findVarCexp a

// Find all variables in input program
let findVariables (_,_,_,edges) = List.fold (fun a x -> Set.union a (Set.ofList (findVar x))) (Set.ofList []) edges

// Semantic functions

// Semantics for arithmetic expressions
let rec arithSem aexp mem =
    let (varMem,arrMem) = mem
    match aexp with
    | V (Var v) -> match Map.tryFind v varMem with
                   | Some a -> a
                   | None -> failwith "Variable not defined"
    | V (Array(v,i)) -> match Map.tryFind (v,arithSem i mem) arrMem with
                        | Some a -> a
                        | None -> failwith "Variable not defined"
    | Num i -> i
    | Plus (x,y) -> (arithSem x mem) + (arithSem y mem)
    | Minus (x,y) -> (arithSem x mem) - (arithSem y mem)
    | Div (x,y) -> (arithSem x mem) / (arithSem y mem)
    | Mult (x,y) -> (arithSem x mem) * (arithSem y mem)
    | Pow (x,y) -> let z2 = arithSem y mem
                   if z2 < 0 then failwith "Power cannot be negative"
                   pown (arithSem x mem) z2
    | UMinus x -> - arithSem x mem

// Semantics for boolean expressions
let rec boolSem bexp mem = 
    match bexp with
    | True -> true
    | False -> false
    | Eq (a1, a2) -> arithSem a1 mem = arithSem a2 mem
    | Geq (a1, a2) -> arithSem a1 mem >= arithSem a2 mem
    | Gt (a1, a2) -> arithSem a1 mem > arithSem a2 mem
    | Leq (a1, a2) -> arithSem a1 mem <= arithSem a2 mem
    | Lt (a1, a2) ->  arithSem a1 mem < arithSem a2 mem
    | Neq (a1, a2) -> arithSem a1 mem <> arithSem a2 mem
    | Not b -> not (boolSem b mem)
    | SAnd (b1, b2) -> boolSem b1 mem && boolSem b2 mem
    | SOr (b1, b2) -> boolSem b1 mem || boolSem b2 mem
    | And (b1, b2) -> let z1 = boolSem b1 mem
                      let z2 = boolSem b2 mem
                      z1 && z2
    | Or (b1, b2) -> let z1 = boolSem b1 mem
                     let z2 = boolSem b2 mem
                     z1 || z2

let semantics act mem =
    let (varMem,arrMem) = mem
    match act with
    | S -> mem
    | A (Assign (Var v,a)) -> let evalA = arithSem a mem
                              match Map.tryFind v varMem with
                              | Some _ -> (Map.add v evalA varMem,arrMem)
                              | None -> failwith "Variable not defined"
    | A (Assign (Array(v,i),a)) -> let evalA = arithSem a mem
                                   let evalI = arithSem i mem
                                   match Map.tryFind (v,evalI) arrMem with
                                   | Some _ -> (varMem , Map.add (v,evalI) evalA arrMem)
                                   | None -> failwith "Variable not defined"
    | B b -> if boolSem b mem then mem else failwith "Boolean expression is not true"

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////  
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////  




