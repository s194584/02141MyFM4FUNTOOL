module SignAnalysis

open FM4FUNAST
open FM4FUNCompiler

type Sign = P | M | Z
type Bool = TT | FF
type AbstractMemory = Map<string, Sign> * Map<string, Set<Sign>> // variable memory, array memory
type AnalysisAssignment = Map<Node, Set<AbstractMemory>>

// Define abstract operators
let OpHat set1 op set2 = Set.fold (fun a1 x -> Set.union a1 (Set.fold (fun a2 y -> Set.union a2 (op x y)) (Set.ofList []) set2)) (Set.ofList []) set1

let OpHatSingle op set1 = Set.fold (fun a x -> Set.union a (op x)) (Set.ofList []) set1

let APlus sign1 sign2 = 
    match sign1, sign2 with
    | M, M -> Set.ofList [M]
    | M, Z -> Set.ofList [M]
    | M, P -> Set.ofList [M; Z; P]
    | Z, M -> Set.ofList [M]
    | Z, Z -> Set.ofList [Z]
    | Z, P -> Set.ofList [P]
    | P, M -> Set.ofList [M; Z; P]
    | P, Z -> Set.ofList [P]
    | P, P -> Set.ofList [P]

let AMinus sign1 sign2 = 
    match sign1, sign2 with
    | M, M -> Set.ofList [M; Z; P]
    | M, Z -> Set.ofList [M]
    | M, P -> Set.ofList [M]
    | Z, M -> Set.ofList [P]
    | Z, Z -> Set.ofList [Z]
    | Z, P -> Set.ofList [M]
    | P, M -> Set.ofList [P]
    | P, Z -> Set.ofList [P]
    | P, P -> Set.ofList [M; Z; P]

let AUMinus sign = 
    match sign with
    | P -> Set.ofList [M]
    | M -> Set.ofList [P]
    | _ -> Set.ofList [Z]

let AMult sign1 sign2 = 
    match sign1, sign2 with
    | Z, _ | _, Z -> Set.ofList [Z]
    | P, M | M, P -> Set.ofList [M]
    | _ -> Set.ofList [P]

let ADiv sign1 sign2 = 
    match sign1, sign2 with
    | _, Z -> Set.ofList []
    | Z, _ -> Set.ofList [Z]
    | P, M | M, P -> Set.ofList [M]
    | _ -> Set.ofList [P]

let APow sign1 sign2 = 
    match sign1, sign2 with
    | _, M -> Set.ofList []
    | M, P -> Set.ofList [M; P]
    | _ -> Set.ofList [P]

let AEq sign1 sign2 = 
    match sign1, sign2 with
    | Z, Z -> Set.ofList [TT]
    | sign1, sign2 when sign1 = sign2 -> Set.ofList [TT; FF]
    | _ -> Set.ofList [FF]

let AGt sign1 sign2 = 
    match sign1, sign2 with
    | Z, Z -> Set.ofList [FF]
    | sign1, sign2 when sign1 = sign2 -> Set.ofList [TT; FF]
    | P, _ | Z, M -> Set.ofList [TT]
    | Z, P -> Set.ofList [FF]
    | _ -> Set.ofList [FF]

let AGeq sign1 sign2 = 
    match sign1, sign2 with
    | Z, Z -> Set.ofList [TT]
    | sign1, sign2 when sign1 = sign2 -> Set.ofList [TT; FF]
    | P, _ | Z, M -> Set.ofList [TT]
    | Z, P -> Set.ofList [FF]
    | _ -> Set.ofList [FF]

let AAnd bool1 bool2 = 
    match bool1, bool2 with
    | TT, TT -> Set.ofList [TT]
    | _ -> Set.ofList [FF]

let AOr bool1 bool2 = 
    match bool1, bool2 with
    | FF, FF -> Set.ofList [FF]
    | _ -> Set.ofList [TT]

let ALt sign1 sign2 = 
    match sign1, sign2 with
    | Z, Z -> Set.ofList [FF]
    | sign1, sign2 when sign1 = sign2 -> Set.ofList [TT; FF]
    | P, _ | Z, M -> Set.ofList [FF]
    | Z, P -> Set.ofList [TT]
    | _ -> Set.ofList [TT]

let ALeq sign1 sign2 = 
    match sign1, sign2 with
    | Z, Z -> Set.ofList [TT]
    | sign1, sign2 when sign1 = sign2 -> Set.ofList [TT; FF]
    | P, _ | Z, M -> Set.ofList [FF]
    | Z, P -> Set.ofList [TT]
    | _ -> Set.ofList [TT]

let ANeq sign1 sign2 = 
    match sign1, sign2 with
    | Z, Z -> Set.ofList [FF]
    | sign1, sign2 when sign1 = sign2 -> Set.ofList [TT; FF]
    | _ -> Set.ofList [TT]

let ANeg bool = if bool = TT then Set.ofList [FF] else Set.ofList [TT]


let getSign i = 
    match i with
    | 0 -> Z
    | i when i > 0 -> P
    | _ -> M

// Analysis function, arithmetics
let rec analysisArith aexp mem =
    let (varMem,arrMem) = mem
    match aexp with
    | V (Var v) -> match Map.tryFind v varMem with
                   | Some a -> Set.ofList [a]
                   | None -> failwith "Variable not defined"
    | V (Array(v,i)) -> let indexSet = analysisArith i mem
                        if Set.contains M indexSet then Set.ofList []
                        else 
                            match Map.tryFind v arrMem with
                            | Some a -> a
                            | None -> failwith "Variable not defined"
    | Num i -> Set.ofList [getSign i]
    | Plus (x,y) -> OpHat (analysisArith x mem) APlus (analysisArith y mem)
    | Minus (x,y) -> OpHat (analysisArith x mem) AMinus (analysisArith y mem)
    | Div (x,y) -> OpHat (analysisArith x mem) ADiv (analysisArith y mem) 
    | Mult (x,y) -> OpHat (analysisArith x mem) AMult (analysisArith y mem)
    | Pow (x,y) -> OpHat (analysisArith x mem) APow (analysisArith y mem)
    | UMinus x -> OpHatSingle AUMinus (analysisArith x mem)

// Analysis function, boolean
let rec analysisBool bexp mem = 
    match bexp with
    | True -> Set.ofList [TT]
    | False -> Set.ofList [FF]
    | Eq (x, y) -> OpHat (analysisArith x mem) AEq (analysisArith y mem)
    | Geq (x, y) -> OpHat (analysisArith x mem) AGeq (analysisArith y mem)
    | Gt (x, y) -> OpHat (analysisArith x mem) AGt (analysisArith y mem)
    | Leq (x, y) -> OpHat (analysisArith x mem) ALeq (analysisArith y mem)
    | Lt (x, y) ->  OpHat (analysisArith x mem) ALt (analysisArith y mem)
    | Neq (x, y) -> OpHat (analysisArith x mem) ANeq (analysisArith y mem)
    | Not b -> OpHatSingle ANeg (analysisBool b mem)
    | SAnd (b1, b2) -> let s1 = analysisBool b1 mem
                       let s2 = analysisBool b2 mem
                       Set.union (Set.intersect s1 (Set.ofList [FF])) (OpHat s1 AAnd s2)
                       
    | SOr (b1, b2) -> let s1 = analysisBool b1 mem
                      let s2 = analysisBool b2 mem
                      Set.union (Set.intersect s1 (Set.ofList [TT])) (OpHat s1 AOr s2)

    | And (b1, b2) -> OpHat (analysisBool b1 mem) AAnd (analysisBool b2 mem)
    | Or (b1, b2) -> OpHat (analysisBool b1 mem) AOr (analysisBool b2 mem)

// Analysis function, actions
let analysisAct act memSet =
    match act with
    | S -> memSet
    | A (Assign (Var v,a)) -> Set.fold (fun a1 mem -> let signSet = analysisArith a mem 
                                                      Set.union a1 (Set.fold (fun a2 s -> let (varMem, arrMem) = mem
                                                                                          match Map.tryFind v varMem with
                                                                                          | Some _ -> Set.add (Map.add v s varMem, arrMem) a2
                                                                                          | None -> failwith "Variable not defined" ) (Set.ofList []) signSet)) (Set.ofList []) memSet
    | A (Assign (Array(v,i),a)) -> Set.fold (fun acc mem -> let indexSet = analysisArith i mem
                                                            let aSet = analysisArith a mem
                                                            let (varMem, arrMem) = mem
                                                            if Set.contains M indexSet then acc
                                                            else let ASet = match Map.tryFind v arrMem with 
                                                                            | Some oldA -> oldA
                                                                            | None -> failwith "Variable not defined" 
                                                                 Set.fold (fun acc1 s' -> let removedOneSet = Set.remove s' ASet
                                                                                          Set.fold (fun acc2 s'' -> let addSToRemovedOneSet = (varMem, Map.add v (Set.add s'' removedOneSet) arrMem)
                                                                                                                    let addSToOriginalSet = (varMem, Map.add v (Set.add s'' ASet) arrMem)
                                                                                                                    Set.add addSToOriginalSet (Set.add addSToRemovedOneSet acc2)) acc1 aSet) acc ASet) (Set.ofList []) memSet   
    | B b -> Set.fold (fun a x -> if Set.contains TT (analysisBool b x) then Set.add x a else a) (Set.ofList []) memSet

let addToMap k v map = 
    match Map.tryFind k map with 
    | Some value -> Map.add k (Set.add v value) map
    | None -> failwith "Key not defined"

let unionMap k s map = 
    match Map.tryFind k map with 
    | Some value -> Map.add k (Set.union s value) map
    | None -> failwith "Key not defined"

let rec traverseEdges eList AA didUpdate = if didUpdate then let (newA, newFlag) = List.fold (fun (acc, flag) (qStart, act, qEnd) -> let newM = analysisAct act (Map.find qStart acc)
                                                                                                                                     if not (Set.isSubset newM (Map.find qEnd acc)) then (unionMap qEnd newM acc, true)
                                                                                                                                     else (acc, flag || false)) (AA, false) eList
                                                             traverseEdges eList newA newFlag
                                                        else AA

let createAnalysisAssignment states = List.fold (fun acc s -> Map.add s (Set.ofList []) acc) (Map.ofList []) states


let computeSolution (states, (qStart, _), _, eList) mem = 
    let initialA = createAnalysisAssignment states // Create initial, empty analysis assignment for all nodes in pg
    let AA = addToMap qStart mem initialA // Initial analysis assignment for start node
    traverseEdges eList AA true

