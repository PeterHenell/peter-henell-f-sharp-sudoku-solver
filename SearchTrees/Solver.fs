module Solver
open DataStructure

exception NoSolutionFound of string
exception SolutionWasFound of Node

type Solver() =

    // Right now the internalSolve is not really recursive, it is more of a loop. Todo: Figure out a good way of making it use recursion in a smart way
    member this.solve (rf: Node) =
        let openNodes = []
        let closedNodes = []
        let nodes = []

        let root = rf
        let openNodes =  root :: openNodes
    
        let rec internalSolve (nodesToExamine : List<Node>)  = 
            if nodesToExamine.Length = 0 then 
               raise (NoSolutionFound("Could not find solution even after " + closedNodes.Length.ToString() + " tries" ))

            let node = nodesToExamine.Head // pick up the node we are going to examine
            let closedNodes = node :: closedNodes // add the node to the list of examined nodes
            let nodesToExamine = nodesToExamine.Tail // remove the node from the list of active nodes

            if (node.isGoal) then 
                node.print
                raise(SolutionWasFound(node))
        
            // Get the children of node and remove nodes that already exist in openNodes or closedNodes
            let nodes =  
                node.getChildren 
                |> List.filter(fun t -> not (List.exists(fun e -> t.equalTo e ) nodesToExamine )) 
                |> List.filter(fun t -> not (List.exists(fun e -> t.equalTo e ) closedNodes  ))

            // add these nodes to the end of openNodes
            // add thse to the begining of openNodes to use bredth first search
            internalSolve (nodes @ nodesToExamine)

        internalSolve openNodes