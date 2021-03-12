module FM4FUNCompiler
open FM4FUNAST

(* Create edges function (both deterministic and non-deterministic version) 
   Create done function 
   Create new, fresh nodes *)

type Act = B of bexp | S of cexp | A of cexp
type Node = N of string
type Edges = E of (Node * Act * Node)
type PG = (Node list * (Node * Node) * Act list * Edges list)

(*let rec edgesC' startNode endNode cexp i qSet aSet =
    match cexp with
    | If gc -> edgesGC startNode endNode gcexp i qSet aSet
    | Do gc -> 
              let b = doneGC gcexp
              let (E1, qSet1, aSet1) = edgesGC startNode startNode gc i qSet aSet
              (Set.add E(startNode, b, endNode) E1, qSet1, Set.add b aSet1)
    | C (c1, c2) -> 
                    let q = N(string i)
                    let (E1, qSet1, aSet1) = edgesC startNode q c1 (i + 1) qSet aSet
                    let (E2, qSet2, aSet2) = edgesC q endNode c2 (i + 1) qSet1 aSet1
                    (Set.union E1 E2, Set.add q qSet2, aSet2)
    | _ -> Set.ofList [E (startNode, cexp, endNode)]
and edgesGC' startNode endNode gcexp i qSet aSet = 
    match gcexp with
    | Then (b,c) -> 
                   let q = N(string i)
                   let (E1, qSet1, aSet1) = edgesC q endNode c i qSet aSet
                   (Set.add E(startNode, b, q) E1, Set.add q qSet1, Set.add b aSet1)
    | GC (gc1,gc2) -> 
                   let (E1, qSet1, aSet1) = edgesGC startNode endNode gc1 i qSet aSet
                   let (E2, qSet2, aSet2) = edgesGC startNode endNode gc2 i qSet1 aSet1
                   (Set.union E1 E2, qSet2, aSet2)*)

let rec doneGC gcexp = 
    match gcexp with
    | Then (b,_) -> Not b
    | GC (gc1, gc2) -> And (doneGC gc1, doneGC gc2)

(* Tail recursive version *)
let rec edgesC startNode endNode cexp i qSet aSet eSet =
    match cexp with
    | If gc -> edgesGC startNode endNode gc i qSet aSet eSet
    | Do gc -> 
              let b = B (doneGC gc)
              edgesGC startNode startNode gc i qSet (Set.add b aSet) (Set.add (E(startNode, b, endNode)) eSet)
    | C (c1, c2) -> 
                    let q = N(string i)
                    let (n, qSet1, aSet1, E1) = edgesC startNode q c1 (i + 1) qSet aSet eSet
                    edgesC q endNode c2 n (Set.add q qSet1) aSet1 E1
    | Assign (x,y) -> 
                    let a = A cexp
                    (i, qSet, Set.add a aSet, Set.ofList [E (startNode, a, endNode)])
    | Skip -> 
            let s = S cexp
            (i, qSet, Set.add s aSet, Set.ofList [E (startNode, s, endNode)])
and edgesGC startNode endNode gcexp i qSet aSet eSet = 
    match gcexp with
    | Then (b,c) -> 
                   let q = N(string i)
                   let b1 = B b
                   edgesC q endNode c (i + 1) (Set.add q qSet) (Set.add b1 aSet) (Set.add (E(startNode, b1, q)) eSet)
    | GC (gc1,gc2) -> 
                   let (n, qSet1, aSet1, E1) = edgesGC startNode endNode gc1 i qSet aSet eSet
                   edgesGC startNode endNode gc2 n qSet1 aSet1 E1

let constructPG cexp = 
    let (i, qSet, aSet, eSet) = edgesC (N("start")) (N("end")) cexp 1 (Set.ofList []) (Set.ofList []) (Set.ofList [])
    (N("start")::N("end")::(Set.toList qSet), (N("start"), N("end")), Set.toList aSet, Set.toList eSet)


