module DataStructure2

open System.Threading.Tasks

type SudokuProblemComplete(board : array<int>) =
    
    // Regions
    // 1; 2; 3
    // 4; 5; 6
    // 7; 8; 9
    member this.getRegion r =
        let nb n = 
            board.[n]
        
        let r = r - 1

        // The starting position for every region
        let m = (((r / 3) * (3 * 9)) + ((r % 3) * 3))
        
        [|
        nb m         ; nb ( m + 1)      ; nb (m + 2)
        nb (m + 9)   ; nb ( m + 1 + 9)  ; nb (m + 2 + 9)
        nb (m + 18)  ; nb ( m + 1 + 18) ; nb (m + 2 + 18)
        |]

    member this.getRow r =
        let r = r - 1
        [| for i in 0..8 -> board.[(i + r * 9)] |]

    member this.getCol c =
        let c = c - 1
        [| for i in 0..8 -> board.[(c + i * 9)] |] 

    

    member this.printTests =
        for i in 0..80 do
            printf "%O ;" (board.[ i ] )
            if (i + 1) % 9 = 0 then
                printfn ""
        printfn ""    
    
    member this.Board = 
        board

    member this.findFreeSpot =
        board |> Array.findIndex(fun x -> x = 0) 

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

    let replaceAt index newEl input =
        let ny = Array.copy input
        ny.[index] <- newEl
        ny

    member this.Content = 
        content

    member this.isGoal =
        // if all the slots are taken then we are at the goal
        not (Array.exists(fun x -> x = 0) content.Board)

    member this.getChildren = 
        // find the index of a free spot to place our next number
        let freeSpot = content.findFreeSpot
        // get the row, column and region based on that index
        let (row, column, region) = content.indexToRCR freeSpot

        
        let p = Async.Parallel [
                                async { return content.getRow row }
                                async { return content.getCol column }
                                async { return content.getRegion region }
                                ]
                |> Async.RunSynchronously
                |> Array.concat
         
        // Get all number between 1 and 9 that do not exists in the row nor column nor region
        let possible = 
            (Set.ofArray [|1..9|]) - Set.ofArray( p )

        let neww = Async.Parallel [for i in possible -> 
                                            async { 
                                                return new SudokuNode(replaceAt freeSpot i content.Board)
                                            } ]
                |> Async.RunSynchronously

        neww

    // Nodes are equal if their underlying boards (arrays) are the same
    member this.equalTo (other : SudokuNode) =
        content.Board = other.Content.Board

    member this.print =
        content.printTests
    
    new (content : array<int>) =
        SudokuNode(new SudokuProblemComplete(content))