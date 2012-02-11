module DataStructure2

type SudokuProblemComplete(board : list<int>) =
    
    // Regions
    // 1; 2; 3
    // 4; 5; 6
    // 7; 8; 9



    member this.getRegion r =
        let nb n = 
            List.nth board n
        
        let r = r - 1

        let m = (((r / 3) * (3 * 9)) + ((r % 3) * 3))
        
        [
        nb m       ; nb (  (m )+1)       ; nb ( (m ) + 2)
        nb (m + 9)   ; nb ( ((m )+1 + 9))  ; nb (((m ) + 2 + 9))
        nb (m + 18)  ; nb ( ((m )+1 + 18)) ; nb (((m ) + 2 + 18))
        ]

    member this.getRow r =
        let r = r - 1
        [for i in 0..8 -> List.nth board (i + r * 9)]

    member this.getCol c =
        let c = c - 1
        [for i in 0..8 -> List.nth board (c + i * 9)] 

    

    member this.printTests =
        for i in 0..80 do
            printf "%O ;" (List.nth board i )
            if (i + 1) % 9 = 0 then
                printfn ""
        printfn ""    
    
    member this.Board = 
        board

    member this.findFreeSpot =
        board |> List.findIndex(fun x -> x = 0) 

    // from index number to Row/Column/region
    member this.indexToRCR i =
        let row = (i / 9) + 1
        let column = (i % 9) + 1
        let region =  
            match column with
            | 1 | 2 | 3 -> match row with
                            | 1 | 2 | 3 -> 1
                            | 4 | 5 | 6 -> 4
                            | 7 | 8 | 9 -> 7
                            | _ -> failwith "out of bounds"
            | 4 | 5 | 6 -> match row with
                            | 1 | 2 | 3 -> 2
                            | 4 | 5 | 6 -> 5
                            | 7 | 8 | 9 -> 8
                            | _ -> failwith "out of bounds" 
            | 7 | 8 | 9 -> match row with
                            | 1 | 2 | 3 -> 3
                            | 4 | 5 | 6 -> 6
                            | 7 | 8 | 9 -> 9
                            | _ -> failwith "out of bounds"       
            | _ -> failwith "out of bounds"         
           
        (row, column, region)


type SudokuNode(content : SudokuProblemComplete) =

    // Based on example from here: http://stackoverflow.com/questions/2889961/f-insert-remove-item-from-list
    let replaceAt index newEl input =
        // For each element, we generate a list of elements that should
        // replace the original one - either singleton list or two elements
        // for the specified index
        input |> List.mapi (fun i el -> if i = index then [newEl] else [el])
        |> List.concat

    member this.Content = 
        content

    member this.isGoal =
        // if all the slots are taken then we are at the goal
        not (List.exists(fun x -> x = 0) content.Board)

    member this.getChildren = 
        // find the index of a free spot to place our next number
        let freeSpot = content.findFreeSpot
        // get the row, column and region based on that index
        let (row, column, region) = content.indexToRCR freeSpot

        // Get all number between 1 and 9 that do not exists in the row nor column nor region
        let possible = 
            (Set.ofList [1..9]) - Set.ofList( (content.getRow row) @ (content.getCol column) @ (content.getRegion region))
            |> Set.toList

        // Create a new list of sudokuNodes where all the elements are the same except the freeSpot replaced with one of the possible numbers
        let neww =
            possible
            |> List.map(fun x -> new SudokuNode(new SudokuProblemComplete(replaceAt freeSpot x content.Board)))
            
        neww

    member this.equalTo (other : SudokuNode) =
        content.Board = other.Content.Board

    member this.print =
        content.printTests
