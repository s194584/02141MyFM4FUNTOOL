module ModelChecker

type Configuration = Node * Memory

let rec reach1single (TS:Edges list) (s,mem) = match TS with
   | [] -> Set.ofList []
   | (qstart, a, qend) :: res when qstart = s -> try 
                                                    let mem2 = semantics a mem
                                                    Set.add (qend,mem2) (reach1single res (s,mem))
                                                 with 
                                                 | err -> reach1single res (s,mem)
                                               
   | _ :: res -> reach1single res (s,mem) 

let rec stuckConfigurationChecker TS IC visited toExplore stuck = 
    match toExplore with
    | [] -> stuck
    | x :: res when not (Set.contains x visited) -> let S' =  reach1single TS x
                                                    if (Set.count S' = 0) 
                                                    then stuckConfigurationChecker TS IC (Set.add x visited) res (x :: stuck)
                                                    else stuckConfigurationChecker TS IC (Set.add x visited) (Set.toList(Set.union (Set.ofList res) S')) stuck
    | _ :: res -> stuckConfigurationChecker TS IC visited res stuck

let stuckConfigurationCheckerTest TS I stepsLeft =
   let rec helper TS visited toExplore stuck stepsLeft=
      if (Set.isEmpty toExplore) || (stepsLeft = 0) then
         stuck
      else
         let (v,e,s) = Set.fold (fun (nV,nE,nS) exState -> 
            if Set.contains exState visited then 
               (nV,nE,nS)
            else 
               let S' = reach1single TS exState
               let nnV = Set.add exState nV
               if Set.isEmpty S' then 
                  (nnV,nE,Set.add exState nS)
               else
                  (nnV,Set.union nE S',nS)) (visited, Set.ofList [], stuck) toExplore
         helper TS v e s (stepsLeft - 1)
   helper TS (Set.ofList []) I (Set.ofList []) stepsLeft

