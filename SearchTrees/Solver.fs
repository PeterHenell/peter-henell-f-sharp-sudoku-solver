module Solver
open DataStructure2

exception NoSolutionFound of string
exception SolutionWasFound of SudokuNode

type Solver() =

    // Right now the internalSolve is not really recursive, it is more of a loop. Todo: Figure out a good way of making it use recursion in a smart way
    member this.solve (root: SudokuNode) =
        let rec internalSolve (nodesToExamine : List<SudokuNode>) (closedNodes : List<SudokuNode>)  = 
            
            if nodesToExamine.Length = 0 then 
               raise (NoSolutionFound("Could not find solution even after " + closedNodes.Length.ToString() + " tries" ))

            let node = nodesToExamine.Head // pick up the node we are going to examine

            if node.isGoal then 
                node.print
                raise(SolutionWasFound(node))
        
            // Get the children of node and remove nodes that already exist in openNodes or closedNodes
            let nodes =  
                node.getChildren 
                |> List.filter(fun t -> not (List.exists(fun e -> t.equalTo e ) nodesToExamine )) 
                |> List.filter(fun t -> not (List.exists(fun e -> t.equalTo e ) closedNodes  ))

            // add these nodes to the end of openNodes
            // add thse to the begining of openNodes to use bredth first search
            internalSolve (nodes @ nodesToExamine.Tail) (node::closedNodes)

        internalSolve (root :: []) []
