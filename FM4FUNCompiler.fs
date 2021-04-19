module FM4FUNCompiler
open FM4FUNAST

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////// THIS IS FOR TASK 2 ///////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
 
// Type definitions for program graph components
type Act = B of bexp | S | A of cexp
type Node = N of string
type Edges = (Node * Act * Node)
type PG = (Node list * (Node * Node) * Act list * Edges list)
type PGType = Det | NonDet | Undef

// Done function (as defined in [Formal Methods, Chapter 2.2 p. 19])
let rec doneGC gcexp = 
    match gcexp with
    | Then (b,_) -> Not b
    | GC (gc1, gc2) -> And (doneGC gc1, doneGC gc2)

// Non-deterministic version of edges function (as defined in [Formal Methods, Chapter 2.2 p. 18])
let rec edgesC startNode endNode cexp i qSet aSet eSet =
    match cexp with
    | If gc -> edgesGC startNode endNode gc i qSet aSet eSet

    | Do gc -> let (e,ni) = edgesGC startNode startNode gc i qSet aSet eSet
               let b = doneGC gc
               ((Set.add (startNode, B b, endNode) e), ni)

    | C (c1, c2) -> let q = N("q" + string i)
                    let (E1, ni) = edgesC startNode q c1 (i+1) qSet aSet eSet
                    let (E2, nni) = edgesC q endNode c2 (ni) qSet aSet eSet
                    (Set.union E1 E2, nni)

    | Assign (x,y) -> let a = A cexp
                      (Set.ofList [(startNode, a, endNode)], i)

    | Skip -> let s = S
              (Set.ofList [(startNode, s, endNode)], i)

and edgesGC startNode endNode gcexp i qSet aSet eSet = 
    match gcexp with
    | Then (b,c) ->   let q = N("q" + string i)
                      let (E, ni) = edgesC q endNode c (i+1) qSet aSet eSet
                      (Set.add (startNode, B b, q) E, ni)

    | GC (gc1,gc2) -> let (E1,ni) = edgesGC startNode endNode gc1 i qSet aSet eSet
                      let (E2,nni) = edgesGC startNode endNode gc2 ni qSet aSet eSet
                      (Set.union E1 E2, nni)



// Deterministic version of edges function (modified as in [Formal Methods, Chapter 2.4 p. 25])
let rec edgesCD startNode endNode cexp i qSet aSet eSet d =
    match cexp with
    | If gc -> let (e,d, ni) = edgesGCD startNode endNode gc i qSet aSet eSet False
               (e, ni)

    | Do gc -> let (e,d,ni) = edgesGCD startNode startNode gc i qSet aSet eSet False
               ((Set.add (startNode, B (Not d), endNode) e), ni)

    | C (c1, c2) ->   let q = N("q" + string i)
                      let (E1, ni) = edgesCD startNode q c1 (i+1) qSet aSet eSet d
                      let (E2, nni) = edgesCD q endNode c2 (ni) qSet aSet eSet d
                      (Set.union E1 E2, nni)

    | Assign (x,y) -> let a = A cexp
                      (Set.ofList [(startNode, a, endNode)], i)

    | Skip ->  let s = S
               (Set.ofList [(startNode, s, endNode)], i)

and edgesGCD startNode endNode gcexp i qSet aSet eSet d = 
    match gcexp with
    | Then (b,c) -> let q = N("q" + string i)
                    let b1 = B(And (b, Not d))
                    let (E, ni) = edgesCD q endNode c (i+1) qSet aSet eSet d
                    (Set.add (startNode, b1, q) E, Or(b, d), ni)

    | GC (gc1,gc2) -> let (E1,d1,ni) = edgesGCD startNode endNode gc1 i qSet aSet eSet d
                      let (E2,d2,nni) = edgesGCD startNode endNode gc2 ni qSet aSet eSet d1
                      (Set.union E1 E2, d2, nni)

// Function to determine whether graph should be deterministic or non-deterministic
let edges cexp flag = 
    match flag with
    | Det ->    let (E,_) = edgesCD (N("qstart")) (N("qend")) cexp 1 (Set.ofList []) (Set.ofList []) (Set.ofList []) False
                E

    | NonDet -> let (E,_) = edgesC (N("qstart")) (N("qend")) cexp 1 (Set.ofList []) (Set.ofList []) (Set.ofList [])
                E

//edge to state+act
let eToSA (qs, act, qe) = ([qs; qe], act)

// Get states and acts from edges
let getStatesAndActs edges = Set.fold (fun (states, acts) e -> let (s, act) = eToSA e
                                                               (Set.union (Set.ofList s) states, Set.add act acts)
                                                               ) (Set.ofList [], Set.ofList []) edges
// Funtion for computing program graph (as defined in [Formal Methods, Chapter 1.1 p. 2])
let constructPG cexp flag =
    let eSet = edges cexp flag
    let (qSet, aSet) = getStatesAndActs eSet
    (N("qstart")::N("qend")::(Set.toList qSet), (N("qstart"), N("qend")), Set.toList aSet, Set.toList eSet)



////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////  
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////  