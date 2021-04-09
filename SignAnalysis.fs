module SignAnalysis
open FM4FUNAST

type Sign = Plus | Minus | Zero
type Bool = TT | FF
type AbstractMemory = Map<string, Sign> * Map<string, Sign set>

// Define abstract operators
let OpHat set1 op set2 = Set.fold (fun a1 x -> Set.union a1 (Set.fold (fun a2 y -> Set.union a2 (op x y)) (Set.ofList []) set2)) (Set.ofList []) set1

let OpHatSingle op set1 = Set.fold (fun a x -> Set.union a (op x)) (Set.ofList []) set1

let APlus sign1 sign2 = 
    match sign1, sign2 with
    | Minus, Minus -> Set.ofList [Minus]
    | Minus, Zero -> Set.ofList [Minus]
    | Minus, Plus -> Set.ofList [Minus; Zero; Plus]
    | Zero, Minus -> Set.ofList [Minus]
    | Zero, Zero -> Set.ofList [Zero]
    | Zero, Plus -> Set.ofList [Plus]
    | Plus, Minus -> Set.ofList [Minus; Zero; Plus]
    | Plus, Zero -> Set.ofList [Plus]
    | Plus, Plus -> Set.ofList [Plus]

let AMinus sign1 sign2 = 
    match sign1, sign2 with
    | Minus, Minus -> Set.ofList [Minus; Zero; Plus]
    | Minus, Zero -> Set.ofList [Minus]
    | Minus, Plus -> Set.ofList [Minus]
    | Zero, Minus -> Set.ofList [Plus]
    | Zero, Zero -> Set.ofList [Zero]
    | Zero, Plus -> Set.ofList [Minus]
    | Plus, Minus -> Set.ofList [Plus]
    | Plus, Zero -> Set.ofList [Plus]
    | Plus, Plus -> Set.ofList [Minus; Zero; Plus]

let AUMinus sign = 
    match sign with
    | Plus -> Minus
    | Minus -> Plus
    | _ -> Zero

let AMult sign1 sign2 = 
    match sign1, sign2 with
    | Zero, _ | _, Zero -> Set.ofList [Zero]
    | Plus, Minus | Minus, Plus -> Set.ofList [Minus]
    | _ -> Set.ofList [Plus]

let ADiv sign1 sign2 = 
    match sign1, sign2 with
    | _, Zero -> Set.ofList []
    | Zero, _ -> Set.ofList [Zero]
    | Plus, Minus | Minus, Plus -> Set.ofList [Minus]
    | _ -> Set.ofList [Plus]

let APow sign1 sign2 = 
    match sign1, sign2 with
    | _, Minus -> Set.ofList []
    | Minus, Plus -> Set.ofList [Minus; Plus]
    | _ -> Set.ofList [Plus]

let AEq sign1 sign2 = 
    match sign1, sign2 with
    | Zero, Zero -> Set.ofList [TT]
    | sign1, sign2 when sign1 = sign2 -> Set.ofList [TT; FF]
    | _ -> Set.ofList [FF]

let AGt sign1 sign2 = 
    match sign1, sign2 with
    | Zero, Zero -> Set.ofList [FF]
    | sign1, sign2 when sign1 = sign2 -> Set.ofList [TT; FF]
    | Plus, _ | Zero, Minus -> Set.ofList [TT]
    | Zero, Plus -> Set.ofList [FF]
    | _ -> Set.ofList [FF]

let AGeq sign1 sign2 = 
    match sign1, sign2 with
    | Zero, Zero -> Set.ofList [TT]
    | sign1, sign2 when sign1 = sign2 -> Set.ofList [TT; FF]
    | Plus, _ | Zero, Minus -> Set.ofList [TT]
    | Zero, Plus -> Set.ofList [FF]
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
    | Zero, Zero -> Set.ofList [FF]
    | sign1, sign2 when sign1 = sign2 -> Set.ofList [TT; FF]
    | Plus, _ | Zero, Minus -> Set.ofList [FF]
    | Zero, Plus -> Set.ofList [TT]
    | _ -> Set.ofList [TT]

let ALeq sign1 sign2 = 
    match sign1, sign2 with
    | Zero, Zero -> Set.ofList [TT]
    | sign1, sign2 when sign1 = sign2 -> Set.ofList [TT; FF]
    | Plus, _ | Zero, Minus -> Set.ofList [FF]
    | Zero, Plus -> Set.ofList [TT]
    | _ -> Set.ofList [TT]

let ANeq sign1 sign2 = 
    match sign1, sign2 with
    | Zero, Zero -> Set.ofList [FF]
    | sign1, sign2 when sign1 = sign2 -> Set.ofList [TT; FF]
    | _ -> Set.ofList [TT]

let ANeg bool = if bool = TT then Set.ofList [FF] else Set.ofList [TT]


let getSign i = 
    match i with
    | 0 -> Zero
    | i when i > 0 -> Plus
    | _ -> Minus


// Analysis function, arithmetics
let rec analysisArith aexp mem =
    let (varMem,arrMem) = mem
    match aexp with
    | V (Var v) -> match Map.tryFind v varMem with
                   | Some a -> SetofList [a]
                   | None -> failwith "Variable not defined"
    | V (Array(v,i)) -> let indexSet = analysisArith i mem
                        if Set.contains Minus indexSet then Set.ofList []
                        else 
                            match Map.tryFind v arrMem with
                            | Some a -> a
                            | None -> failwith "Variable not defined"
    | Num i -> getSign i
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
    | Eq (a1, a2) -> OpHat (analysisArith x mem) AEq (analysisArith y mem)
    | Geq (a1, a2) -> OpHat (analysisArith x mem) AGeq (analysisArith y mem)
    | Gt (a1, a2) -> OpHat (analysisArith x mem) AGt (analysisArith y mem)
    | Leq (a1, a2) -> OpHat (analysisArith x mem) ALeq (analysisArith y mem)
    | Lt (a1, a2) ->  OpHat (analysisArith x mem) ALt (analysisArith y mem)
    | Neq (a1, a2) -> OpHat (analysisArith x mem) ANeq (analysisArith y mem)
    | Not b -> OpHatSingle ANeg (analysisBool b mem)
    | SAnd (b1, b2) -> let s1 = analysisBool b1 mem
                       let s2 = analysisBool b2 mem
                       Set.union (Set.intersect s1 (Set.ofList [FF])) (OpHat (analysisBool b1 mem) AAnd (analysisBool b2 mem))
                       
    | SOr (b1, b2) -> let s1 = analysisBool b1 mem
                      let s2 = analysisBool b2 mem
                      Set.union (Set.intersect s1 (Set.ofList [TT])) (OpHat (analysisBool b1 mem) AOr (analysisBool b2 mem))

    | And (b1, b2) -> OpHat (analysisBool b1 mem) AAnd (analysisBool b2 mem)
    | Or (b1, b2) -> OpHat (analysisBool b1 mem) AOr (analysisBool b2 mem)

let analysisAct act memSet =
    match act with
    | S -> mem
    | A (Assign (Var v,a)) -> Set.fold (fun a1 mem -> let signSet = analysisArith a mem 
                                                      Set.union a1 (Set.fold (fun a2 s -> let (varMem, arrMem) = mem
                                                                                          match Map.tryFind v varMem with
                                                                                          | Some _ -> Set.add (Map.add v s varMem, arrMem) a2
                                                                                          | None -> failwith "Variable not defined" ) (Set.ofList []) signSet)) (Set.ofList []) memSet
    | A (Assign (Array(v,i),a)) -> Set.fold (fun acc mem -> let indexSet = analysisArith i mem
                                                           let aSet = analysisArith a mem
                                                           let (varMem, arrMem) = mem
                                                           if Set.contains Minus indexSet then a1
                                                           else let ASet = match Map.tryFind v arrMem with 
                                                                           | Some oldA -> oldA
                                                                           | None -> failwith "Variable not defined" 
                                                                Set.fold (fun acc1 s' -> let removedOneSet = Set.remove s' ASet
                                                                                        Set.fold (fun acc2 s'' -> let addSToRemovedOneSet = Set.add s'' removedOneSet
                                                                                                                  let addSToOriginalSet = Set.add s'' ASet
                                                                                                                  Set.add addSToOriginalSet (Set.add addSToRemovedOneSet acc2)) acc1 aSet) acc ASet) (Set.ofList []) memSet   
    | B b -> Set.fold (fun a x -> if Set.contains TT (analysisBool b x) then Set.add x a else a) (Set.ofList []) memSet