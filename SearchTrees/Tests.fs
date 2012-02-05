module NodeTests

open DataStructure
open NUnit.Framework
open FsUnit

[<TestFixture>]
type NodeFixture() =
    let nStart = new Node(new SudokuProblem(1, 0, 0, 0, 0, 0, 0, 0, 0) )
    let nGoal = new Node(new SudokuProblem(1, 2, 3, 4, 5, 6, 7, 8, 9) )
    let valueInMiddleNode = new Node(new SudokuProblem(0, 0, 0, 0, 5, 0, 0, 0, 0) )
    let threeValuesNode = new Node(new SudokuProblem(0, 4, 0, 0, 5, 0, 3, 0, 0) )
    let almostGoalNode = new Node(new SudokuProblem(1, 4, 2, 6, 5, 7, 3, 9, 0) )

    [<Test>]
    member this.
        ``Based on known states of the Node isGoal should be true``() =

       nStart.isGoal |> should equal false
       nGoal.isGoal |> should equal true

    [<Test>]
    member this.
        ``Should create 8 child nodes based on a node with one value``() =
        
        let children = nStart.getChildren
        let randomChildren = valueInMiddleNode.getChildren

        children |> should haveLength 8
        randomChildren |> should haveLength 8

    [<Test>]
    member this.
        ``Should create some child nodes based on a node with some values``() =
        
        let someChildren = threeValuesNode.getChildren
        someChildren |> should haveLength 6

    [<Test>]
    member this.
        ``Should create one child node when all but one slots are filled``() =
        
        let oneChild = almostGoalNode.getChildren
        oneChild |> should haveLength 1

    [<Test>]
    member this.
        ``Should create goal child node when all but one slots are filled``() =
        
        let goalChild = almostGoalNode.getChildren
        goalChild.Head.isGoal |> should equal true
    
    [<Test>]
    member this.
        ``Two nodes should only be equal if they have the same values in the same spots``() =
        
        let first = new Node(new SudokuProblem(1, 0, 0, 0, 0, 0, 0, 0, 0) )
        let second = new Node(new SudokuProblem(1, 0, 0, 0, 0, 0, 0, 0, 0) )
        first.equalTo second |> should equal true
        first.equalTo first |> should equal true

        let first = new Node(new SudokuProblem(1, 2, 3, 4, 5, 6, 7, 0, 0) )
        let second = new Node(new SudokuProblem(1, 0, 0, 0, 0, 0, 0, 0, 0) )
        first.equalTo second |> should equal false

        let first = new Node(new SudokuProblem(0, 1, 0, 0, 0, 0, 0, 0, 0) )
        let second = new Node(new SudokuProblem(1, 0, 0, 0, 0, 0, 0, 0, 0) )
        first.equalTo second |> should equal false
    


    [<Test>]
    member this.
        ``isGoal should only validate valid goals``() =

        let first = new Node(new SudokuProblem(1, 2, 3, 4, 5, 6, 7, 8, 9) )
        let second = new Node(new SudokuProblem(1, 1, 1, 1, 1, 1, 1, 1, 1) )

        first.isGoal |> should equal true
        second.isGoal |> should equal false

//[<TestFixture>]
//type SudokuProblemFixture() =

    [<Test>]
    member this.
        ``Should Find Spots For Each Slot Of The List based on the previous step``() =
        let step1 = new SudokuProblem(1, 0, 0, 0, 0, 0, 0, 0, 0)
        let step2 = SudokuProblem.FromList (step1.findSpotFor 2)
        let step3 = SudokuProblem.FromList (step2.findSpotFor 3)
        let step4 = SudokuProblem.FromList (step3.findSpotFor 4)
        let step5 = SudokuProblem.FromList (step4.findSpotFor 5)
        let step6 = SudokuProblem.FromList (step5.findSpotFor 6)
        let step7 = SudokuProblem.FromList (step6.findSpotFor 7)
        let step8 = SudokuProblem.FromList (step7.findSpotFor 8)
        let step9 = SudokuProblem.FromList (step8.findSpotFor 9)

        step1.toList |> should equal [1; 0; 0; 0; 0; 0; 0; 0; 0]
        step2.toList |> should equal [1; 2; 0; 0; 0; 0; 0; 0; 0]
        step3.toList |> should equal [1; 2; 3; 0; 0; 0; 0; 0; 0]
        step4.toList |> should equal [1; 2; 3; 4; 0; 0; 0; 0; 0]
        step5.toList |> should equal [1; 2; 3; 4; 5; 0; 0; 0; 0]
        step6.toList |> should equal [1; 2; 3; 4; 5; 6; 0; 0; 0]
        step7.toList |> should equal [1; 2; 3; 4; 5; 6; 7; 0; 0]
        step8.toList |> should equal [1; 2; 3; 4; 5; 6; 7; 8; 0]
        step9.toList |> should equal [1; 2; 3; 4; 5; 6; 7; 8; 9]


    [<Test>]
    member this.
        ``Should find spots on the list when there is one spot taken in the middle of the list``() = 
        let step = new SudokuProblem(0, 0, 0, 0, 5, 0, 0, 0, 0)
        let step2 = SudokuProblem.FromList (step.findSpotFor 1)
        step2.toList |> should equal [1; 0; 0; 0; 5; 0; 0; 0; 0]

    [<Test>]
    member this.
        ``Should find spots for a number that already exists (Should not care about the number itself)``() = 
        let step = new SudokuProblem(0, 0, 0, 0, 5, 0, 0, 0, 0)
        let step2 = SudokuProblem.FromList (step.findSpotFor 5)

        step2.toList |> should equal [5; 0; 0; 0; 5; 0; 0; 0; 0]

    [<Test>]
    member this.
        ``Should find a spot for a number when there are no numbers in the list``() = 
        let step = new SudokuProblem(0, 0, 0, 0, 0, 0, 0, 0, 0)
        let step2 = SudokuProblem.FromList (step.findSpotFor 5)

        step2.toList |> should equal [5; 0; 0; 0; 0; 0; 0; 0; 0]

    [<Test>]
    member this.
        ``Should create empty list when trying to find a slot for a number when the list is full``() = 
        let step = new SudokuProblem(5, 5, 5, 5, 5, 5, 5, 5, 5)
        let step2 = (step.findSpotFor 5)

        match step2 with
        | None -> "Success" |> should equal "Success"
        | Some(a) -> failwith "There should not have been any results from this"

    [<Test>]
    member this.
        ``Should throw exception when trying to create a SudokuProblem from an empty list``() = 
        let step = new SudokuProblem(5, 5, 5, 5, 5, 5, 5, 5, 5)
        let step2 = (step.findSpotFor 5)        
        
        (fun () ->
            SudokuProblem.FromList step2 |> ignore )
            |> should throw typeof<System.ArgumentException>