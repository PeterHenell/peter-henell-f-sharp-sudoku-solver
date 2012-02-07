module DataStructure2


type SudokuProblemComplete(board : list<int>) =
    
    // Regions
    // 1; 2; 3
    // 4; 5; 6
    // 7; 8; 9

    let getRegion r =
        let r = r - 1
        [
        (List.nth board  (0 + (r * 3)))        ; (List.nth board  (1+(r * 3)))       ; (List.nth board  (2+(r * 3)))
        (List.nth board ((0 + (r * 3)) + 9))   ; (List.nth board ((1+(r * 3)) + 9))  ; (List.nth board ((2+(r * 3)) + 9))
        (List.nth board ((0 + (r * 3)) + 18))  ; (List.nth board ((1+(r * 3)) + 18)) ; (List.nth board ((2+(r * 3)) + 18))
        ]

    let getRow r =
        let r = r - 1
        [for i in 0..8 -> List.nth board (i + r * 9)]

    let getCol c =
        let c = c - 1
        [for i in 0..8 -> List.nth board (c + i * 9)] 

    

    let isIn lst i = 
        lst |> List.exists(fun x -> x = i) 

    let isInCol c i =
        isIn c i

    let isInRow r i =
        isIn r i

    let isInRegion r i =
        isIn r i

    member this.PrintTests =
        printfn "Regions"
        for i in [1..9] do
            printfn "%O" (getRegion i)
        
        printfn "Rows"
        for i in [1..9] do
            printfn "%O" (getRow i)

        printfn "Cols"
        for i in [1..9] do
            printfn "%O" (getCol i)
    
    member this.Board = 
        board

type SudokuNode(content : SudokuProblemComplete) =

    member this.Content = 
        content

    member this.isGoal =
        false

    member this.getChildren = 
        [ new SudokuNode(content) ]

    member this.equalTo (other : SudokuNode) =
        content.Board = other.Content.Board

    member this.print =
        printfn "N/A"
