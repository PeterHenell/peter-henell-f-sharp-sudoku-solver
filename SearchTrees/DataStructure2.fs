module DataStructure2


type SudokuProblemComplete(board : list<int>) =
    
    // Regions
    // 1; 2; 3
    // 4; 5; 6
    // 7; 8; 9

    let getRegion r =
        let r = r - 1
        [
        (List.nth board  (0+r))        ; (List.nth board  (1+r))       ; (List.nth board  (2+r))
        (List.nth board ((0+r) + 9))   ; (List.nth board ((1+r) + 9))  ; (List.nth board ((2+r) + 9))
        (List.nth board ((0+r) + 18))  ; (List.nth board ((1+r) + 18)) ; (List.nth board ((2+r) + 18))
        ]

    let getRow r =
        let r = r - 1
        [
            List.nth board (0 + (r * 9)) 
            List.nth board (1 + (r * 9))
            List.nth board (2 + (r * 9))
            List.nth board (3 + (r * 9))
            List.nth board (4 + (r * 9))
            List.nth board (5 + (r * 9))
            List.nth board (6 + (r * 9))
            List.nth board (7 + (r * 9))
            List.nth board (8 + (r * 9))
        ]

    let getCol c =
        let c = c - 1
        [ 
            List.nth board c
            List.nth board (c + 9 * 1)
            List.nth board (c + 9 * 2)
            List.nth board (c + 9 * 3)
            List.nth board (c + 9 * 4)
            List.nth board (c + 9 * 5)
            List.nth board (c + 9 * 6)
            List.nth board (c + 9 * 7)
            List.nth board (c + 9 * 8)
        ]
    

    let isIn lst i = 
        lst |> List.exists(fun x -> x = i) 

    let isInCol c i =
        isIn c i

    let isInRow r i =
        isIn r i

    let isInRegion r i =
        isIn r i


type SudokuNode(content : SudokuProblemComplete) =

    member this.Content = 
        content

    member this.isGoal =
        false

    member this.getChildren = 
        [content]

    member this.equalTo other =
        false

    member this.print =
        printfn "N/A"
