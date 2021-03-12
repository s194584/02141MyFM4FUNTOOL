module FM4FUNCompiler
open FM4FUNAST

(* Create edges function (both deterministic and non-deterministic version) 
   Create done function 
   Create new, fresh nodes *)

type Act = B of bexp | S of cexp | A of cexp
type Node = N of string
type Edges = E of (Node * Act * Node)
type PG = (Node list * (Node * Node) * Act list * Edges list)
type PGType = Det | NonDet

let rec doneGC gcexp = 
    match gcexp with
    | Then (b,_) -> Not b
    | GC (gc1, gc2) -> And (doneGC gc1, doneGC gc2)

(* Non-deterministic version of edges function *)
let rec edgesC startNode endNode cexp i qSet aSet eSet =
    match cexp with
    | If gc -> edgesGC startNode endNode gc i qSet aSet eSet
    | Do gc -> 
              let b = B (doneGC gc)
              edgesGC startNode startNode gc i qSet (Set.add b aSet) (Set.add (E(startNode, b, endNode)) eSet)
    | C (c1, c2) -> 
                    let q = N("q" + string i)
                    let (n, qSet1, aSet1, E1) = edgesC startNode q c1 (i + 1) qSet aSet eSet
                    edgesC q endNode c2 n (Set.add q qSet1) aSet1 E1
    | Assign (x,y) -> 
                    let a = A cexp
                    (i, qSet, Set.add a aSet, Set.add (E(startNode, a, endNode)) eSet)
    | Skip -> 
            let s = S cexp
            (i, qSet, Set.add s aSet, Set.add (E(startNode, s, endNode)) eSet)
and edgesGC startNode endNode gcexp i qSet aSet eSet = 
    match gcexp with
    | Then (b,c) -> 
                   let q = N("q" + string i)
                   let b1 = B b
                   edgesC q endNode c (i + 1) (Set.add q qSet) (Set.add b1 aSet) (Set.add (E(startNode, b1, q)) eSet)
    | GC (gc1,gc2) -> 
                   let (n, qSet1, aSet1, E1) = edgesGC startNode endNode gc1 i qSet aSet eSet
                   edgesGC startNode endNode gc2 n qSet1 aSet1 E1

(* Deterministic version of edges function *)
let rec edgesCD startNode endNode cexp i qSet aSet eSet d =
    match cexp with
    | If gc -> edgesGCD startNode endNode gc i qSet aSet eSet False
    | Do gc -> 
              edgesGCD startNode startNode gc i qSet (Set.add (B (Not d)) aSet) (Set.add (E(startNode, B (Not d), endNode)) eSet) False
    | C (c1, c2) -> 
                    let q = N("q" + string i)
                    let (n, qSet1, aSet1, E1, d) = edgesCD startNode q c1 (i + 1) qSet aSet eSet d
                    edgesCD q endNode c2 n (Set.add q qSet1) aSet1 E1 d
    | Assign (x,y) -> 
                    let a = A cexp
                    (i, qSet, Set.add a aSet, Set.add (E(startNode, a, endNode)) eSet, d)
    | Skip -> 
            let s = S cexp
            (i, qSet, Set.add s aSet, Set.add (E(startNode, s, endNode)) eSet, d)
and edgesGCD startNode endNode gcexp i qSet aSet eSet d = 
    match gcexp with
    | Then (b,c) -> 
                   let q = N("q" + string i)
                   let b1 = B(And (b, Not d))
                   edgesCD q endNode c (i + 1) (Set.add q qSet) (Set.add b1 aSet) (Set.add (E(startNode, b1, q)) eSet) (Or(b, d))
    | GC (gc1,gc2) -> 
                   let (n, qSet1, aSet1, E1, d1) = edgesGCD startNode endNode gc1 i qSet aSet eSet d
                   edgesGCD startNode endNode gc2 n qSet1 aSet1 E1 d1

let edges cexp flag = 
    match flag with
    | Det -> let (i, qSet, aSet, eSet, d) = edgesCD (N("qstart")) (N("qend")) cexp 1 (Set.ofList []) (Set.ofList []) (Set.ofList []) False
             (qSet, aSet, eSet)
    | NonDet -> let (i, qSet, aSet, eSet) = edgesC (N("qstart")) (N("qend")) cexp 1 (Set.ofList []) (Set.ofList []) (Set.ofList [])
                (qSet, aSet, eSet)


let constructPG cexp flag =
    let (qSet, aSet, eSet) = edges cexp flag
    (N("qstart")::N("qend")::(Set.toList qSet), (N("qstart"), N("qend")), Set.toList aSet, Set.toList eSet)
