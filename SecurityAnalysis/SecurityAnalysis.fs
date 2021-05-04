module SecurityAnalysis

open FM4FUNAST
open ClassificationAST

type FlowRelation = string*string
type SecurityClassification = Map<string,string>

let calcFRFromSets X Y = Set.fold (fun a1 x -> Set.fold (fun a2 y -> Set.add (x,y) a2) a1 Y) (Set.ofList []) X

// fv to find free variables in aexp and bexp
let rec fvAexp aexp =
    match aexp with
    | V (Var v) -> Set.ofList [v]
    | V (Array(v,i)) -> Set.add v (fvAexp i)
    | Num i -> Set.ofList []
    | Plus (x,y)  | Minus (x,y) | Div (x,y) | Mult (x,y) | Pow (x,y) -> Set.union (fvAexp x) (fvAexp y) 
    | UMinus x -> fvAexp x 
and fvBexp bexp = 
    match bexp with
    | True -> Set.ofList []
    | False -> Set.ofList []
    | Eq (a1, a2) | Geq (a1, a2) | Gt (a1, a2) | Leq (a1, a2) | Lt (a1, a2) | Neq (a1, a2) -> Set.union (fvAexp a1) (fvAexp a2) 
    | Not b -> fvBexp b
    | SAnd (b1, b2) | SOr (b1, b2) | And (b1, b2) | Or (b1, b2) -> Set.union (fvBexp b1) (fvBexp b2)

// Compute security classification
let rec computeSC ca L =
    match ca with
    | ClassAss(ca1, ca2) -> computeSC ca2 (computeSC ca1 L)
    | Class(v, cl) -> Map.add v cl L
    
let unionMap k s map = 
    match Map.tryFind k map with 
    | Some value -> Map.add k (Set.union s value) map
    | None -> failwith "Key not defined"

let addToMap k v map = 
    match Map.tryFind k map with 
    | Some value -> Map.add k (Set.add v value) map
    | None -> Map.add k (Set.ofList [v]) map

// Compute flow by lattice
let rec computeFL lat =
    let rec computeInitialFL lat FL =
        match lat with
        | LatAss(a1, a2) -> computeInitialFL a2 (computeInitialFL a1 FL)
        | LatRel(r1, r2) -> addToMap r2 r2 (addToMap r1 r1 (addToMap r1 r2 FL))

    let rec updateTransitiveFL (FL:Map<'a,Set<'a>>) = 
        //let newFL = Map.fold (fun a cl sCl -> Set.fold (fun a1 cl1 -> Set.union (Map.find cl1 FL) a1) (Set.ofList []) a ) FL FL
        let (newFL:Map<'a,Set<'a>>) = Map.fold (fun aFL cl setCl -> let nextSet = Set.fold (fun aSetCl scl -> Set.union aSetCl (Map.find scl FL)) setCl setCl
                                                                    Map.add cl nextSet aFL) FL FL
        if newFL = FL then FL else updateTransitiveFL newFL
                                  
    updateTransitiveFL (computeInitialFL lat (Map.ofList []))

// Is security classification well-defined?
let isWellDefinedSC vs L = Set.forall (fun v -> match Map.tryFind v L with
                                             | Some _ -> true
                                             | None -> false ) vs

// Find the allowed flow relations
let rec findAllowedFlowRelations vs L FL = Map.fold (fun a var cl -> let allowedClasses = Map.find cl FL
                                                                     Set.fold (fun a1 var1 -> if Set.contains (Map.find var1 L) allowedClasses then Set.add (var,var1) a1 else a1) a vs) (Set.ofList []) L

// sec & sec2 [p. 67-68]
let rec sec cexp X =
    match cexp with
    | Assign (Var v,a) -> calcFRFromSets (Set.union X (fvAexp a)) (Set.ofList [v])
    | Assign (Array(v,i),a) -> calcFRFromSets (Set.union (Set.union X (fvAexp i)) (fvAexp a)) (Set.ofList [v])
    | Skip -> Set.ofList []
    | C(c1, c2) -> Set.union (sec c1 X) (sec c2 X)
    | If(gc) -> let (w,d) = sec2 gc (False,X)
                w
    | Do(gc) -> let (w,d) = sec2 gc (False,X)
                w 
and sec2 gcexp (d,X) =
    match gcexp with
    | Then(b, c) -> let w = sec c ((Set.union (Set.union X (fvBexp b)) (fvBexp d)))
                    (w,Or(b,d))
    | GC(gc1, gc2) -> let (w1,d1) = sec2 gc1 (d,X)
                      let (w2,d2) = sec2 gc1 (d1,X)
                      (Set.union w1 w2, d2)

