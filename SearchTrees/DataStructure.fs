namespace DataStructure
//
//// This defines the datastructure containing the values
//type SudokuProblem(board : (int *int *int *int *int *int *int *int *int))= 
//   //let Board = board
//    
//    member this.toList = 
//        match board with
//        | (a, b, c, d, e, f, g, h, i) -> [a; b; c; d; e; f; g; h; i]
//
//    // find the first available slot (a slot that is zero)
//    member this.findSpotFor value =
//        match board with
//        | (0, b, c, d, e, f, g, h, i) -> Some([value; b; c; d; e; f; g; h; i])
//        | (a, 0, c, d, e, f, g, h, i) -> Some([a; value; c; d; e; f; g; h; i])
//        | (a, b, 0, d, e, f, g, h, i) -> Some([a; b; value; d; e; f; g; h; i])
//        | (a, b, c, 0, e, f, g, h, i) -> Some([a; b; c; value; e; f; g; h; i])
//        | (a, b, c, d, 0, f, g, h, i) -> Some([a; b; c; d; value; f; g; h; i])
//        | (a, b, c, d, e, 0, g, h, i) -> Some([a; b; c; d; e; value; g; h; i])
//        | (a, b, c, d, e, f, 0, h, i) -> Some([a; b; c; d; e; f; value; h; i])
//        | (a, b, c, d, e, f, g, 0, i) -> Some([a; b; c; d; e; f; g; value; i])
//        | (a, b, c, d, e, f, g, h, 0) -> Some([a; b; c; d; e; f; g; h; value])
//        | (a, b, c, d, e, f, g, h, i) -> None
//
//    static member FromList l =
//        match l with
//        | Some([a; b; c; d; e; f; g; h; i]) -> new SudokuProblem(a, b, c, d, e, f, g, h, i)
//        | _ -> raise (System.ArgumentException("Wrong number of items in the supplid list"))
//
//
//// This defines the rules
//type Node(content: SudokuProblem)  =
//
//    member this.Content = 
//        content
//
//    // A child of n is a node where a new value have been added to the array on a legal spot
//    member this.getChildren  =
//        let taken = Set.ofList content.toList
//        let possible = (Set.ofList [1..9]) - taken |> Set.toList
//        
//        // For each possible number, generate a new Node based on the current node
//        possible  
//            |> List.map(fun x -> 
//                    match (content.findSpotFor x) with
//                    | Some(a) -> new Node(SudokuProblem.FromList (Some(a)))
//                    | None -> failwith "Could not find a spot for x"
//               ) 
//
//    member this.equalTo (other : Node) = 
//        other.Content.toList = content.toList
//
//    member this.isGoal =
//        // if the list contain all the numbers between 1 and 9 then we are done
//        (Set.ofList content.toList) = (Set.ofList [1..9])
//    
//    member this.print = 
//        match content.toList with
//        | [a; b; c; d; e; f; g; h; i] -> printfn "%d; %d; %d; %d; %d; %d; %d; %d; %d;" a b c d e f g h i
//        | _ -> printfn ""
//
//    new (content: List<int>) =
//        match content with
//        | [a; b; c; d; e; f; g; h; i] -> new Node(new SudokuProblem(a, b, c, d, e, f, g, h, i))
//        | _ -> Node([])
//
