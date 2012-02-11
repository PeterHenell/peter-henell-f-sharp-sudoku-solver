module NodeTests

open DataStructure
open NUnit.Framework
open FsUnit
open DataStructure2

[<TestFixture>]
type SudokuNodeFixture() =
    let board = [   
                    0; 0; 8; 0; 1; 0; 0; 0; 9;
                    6; 0; 1; 0; 9; 0; 2; 3; 0;
                    0; 4; 0; 0; 3; 7; 0; 0; 5;
                    0; 3; 5; 0; 0; 8; 2; 0; 0;
                    0; 0; 2; 6; 5; 0; 8; 0; 0;
                    0; 0; 4; 0; 0; 1; 7; 5; 0;
                    5; 0; 0; 3; 4; 0; 0; 8; 0;
                    0; 9; 7; 0; 8; 0; 5; 0; 6;
                    1; 0; 0; 0; 6; 0; 9; 0; 0; ]
    let board2 = [   
                    2; 5; 8; 0; 1; 0; 0; 0; 9;
                    6; 0; 1; 0; 9; 0; 2; 3; 0;
                    0; 4; 0; 0; 3; 7; 0; 0; 5;
                    0; 3; 5; 0; 0; 8; 2; 0; 0;
                    0; 0; 2; 6; 5; 0; 8; 0; 0;
                    0; 0; 4; 0; 0; 1; 7; 5; 0;
                    5; 0; 0; 3; 4; 0; 0; 8; 0;
                    0; 9; 7; 0; 8; 0; 5; 0; 6;
                    1; 0; 0; 0; 6; 0; 9; 0; 0; ]
    let fakeGoal = [   
                    2; 5; 8; 4; 1; 4; 4; 4; 9;
                    6; 4; 1; 4; 9; 5; 2; 3; 4;
                    5; 4; 5; 5; 3; 7; 5; 5; 5;
                    5; 3; 5; 5; 5; 8; 2; 5; 5;
                    5; 5; 2; 6; 5; 5; 8; 5; 5;
                    5; 5; 4; 5; 5; 1; 7; 5; 5;
                    5; 5; 5; 3; 4; 5; 5; 8; 5;
                    5; 9; 7; 5; 8; 5; 5; 5; 6;
                    1; 5; 5; 5; 6; 5; 9; 5; 5; ]
    
    let emptyBoard = [
                    0; 0; 0; 0; 0; 0; 0; 0; 0;
                    0; 0; 0; 0; 0; 0; 0; 0; 0;
                    0; 0; 0; 0; 0; 0; 0; 0; 0;
                    0; 0; 0; 0; 0; 0; 0; 0; 0;
                    0; 0; 0; 0; 0; 0; 0; 0; 0;
                    0; 0; 0; 0; 0; 0; 0; 0; 0;
                    0; 0; 0; 0; 0; 0; 0; 0; 0;
                    0; 0; 0; 0; 0; 0; 0; 0; 0;
                    0; 0; 0; 0; 0; 0; 0; 0; 0; ]

    let problem = new SudokuProblemComplete(board)
    let problem2 = new SudokuProblemComplete(board2)


    [<Test>]
    member test.
        ``Should get the list that corresponds to a column``() =
        let a = problem.getCol 1
        let b = problem.getCol 9

        a |> should equal [0;
                           6;
                           0;
                           0;
                           0;
                           0;
                           5;
                           0;
                           1; ]
        
        b |> should equal [9;
                           0; 
                           5;
                           0;
                           0;
                           0;
                           0;
                           6;
                           0; ]
    [<Test>]
    member test.
        ``Should get the list that corresponds to a row``() =
        let a = problem.getRow 1
        let b = problem.getRow 9

        a |> should equal [0; 0; 8; 0; 1; 0; 0; 0; 9; ]
        b |> should equal [1; 0; 0; 0; 6; 0; 9; 0; 0; ]
    
    [<Test>]
    member test.
        ``Should get the list that corresponds to a region``() =
        let a = problem.getRegion 1
        let a2 = problem.getRegion 2
        let a3 = problem.getRegion 3
        let a4 = problem.getRegion 4
        let a5 = problem.getRegion 5
        let a6 = problem.getRegion 6
        let a7 = problem.getRegion 7
        let a8 = problem.getRegion 8
        let a9 = problem.getRegion 9

        a |> should equal [ 0; 0; 8;
                            6; 0; 1;
                            0; 4; 0; ]


        a3 |> should equal [
                            0; 0; 9; 
                            2; 3; 0;
                            0; 0; 5;]


        a4 |> should equal [
                            0; 3; 5;
                            0; 0; 2;
                            0; 0; 4; ]

        a5 |> should equal [
                          0; 0; 8;
                          6; 5; 0;
                          0; 0; 1; ]

        a6 |> should equal [
                            2; 0; 0;
                            8; 0; 0;
                            7; 5; 0; ]

        a7 |> should equal [
                        5; 0; 0; 
                        0; 9; 7;
                        1; 0; 0;]

        a8 |> should equal [
                        3; 4; 0;
                        0; 8; 0;
                        0; 6; 0; ]

        a9 |> should equal [
                           0; 8; 0;
                           5; 0; 6;
                           9; 0; 0; ]




    [<Test>]
    member test.
        ``should find the index of the first free spot``() =

        let firstFreeSpot = problem.findFreeSpot
        let firstFreeSpot2 = problem2.findFreeSpot

        firstFreeSpot |> should equal 0
        firstFreeSpot2 |> should equal 3


    [<Test>]
    member test.
        ``Should convert from index to row/column/region``() =

        let (row, column, region) = problem.indexToRCR 0
        row |> should equal 1
        column |> should equal 1
        region |> should equal 1

        let (row, column, region) = problem.indexToRCR 1
        row |> should equal 1
        column |> should equal 2
        region |> should equal 1

        let (row, column, region) = problem.indexToRCR 3
        row |> should equal 1
        column |> should equal 4
        region |> should equal 2

        let (row, column, region) = problem.indexToRCR 8
        row |> should equal 1
        column |> should equal 9
        region |> should equal 3

        let (row, column, region) = problem.indexToRCR 9
        row |> should equal 2
        column |> should equal 1
        region |> should equal 1

        let (row, column, region) = problem.indexToRCR 27
        row |> should equal 4
        column |> should equal 1
        region |> should equal 4

        let (row, column, region) = problem.indexToRCR 30
        row |> should equal 4
        column |> should equal 4
        region |> should equal 5

        let (row, column, region) = problem.indexToRCR 33
        row |> should equal 4
        column |> should equal 7
        region |> should equal 6

        let (row, column, region) = problem.indexToRCR 35
        row |> should equal 4
        column |> should equal 9
        region |> should equal 6

        let (row, column, region) = problem.indexToRCR 54
        row |> should equal 7
        column |> should equal 1
        region |> should equal 7

        let (row, column, region) = problem.indexToRCR 67
        row |> should equal 8
        column |> should equal 5
        region |> should equal 8

        let (row, column, region) = problem.indexToRCR 80
        row |> should equal 9
        column |> should equal 9
        region |> should equal 9


    [<Test>]
    member test.
        ``Should be goal when all spots are taken``() =

         let fakeGoalNode = new SudokuNode(new SudokuProblemComplete(fakeGoal))
         let notGoalNode =  new SudokuNode(new SudokuProblemComplete(board))

         fakeGoalNode.isGoal |> should equal true
         notGoalNode.isGoal |> should equal false

    [<Test>]
    member test.
        ``Should get children from a node``() =
        let notGoalNode =  new SudokuNode(new SudokuProblemComplete(board))
        let nodes = notGoalNode.getChildren
        
        let emptyNode = new SudokuNode(new SudokuProblemComplete(emptyBoard))
        let childOfEmpty = emptyNode.getChildren

        nodes |> should haveLength 3
        
        childOfEmpty |> should haveLength 9



        let n1 = List.nth childOfEmpty 1
        let n2 = List.nth childOfEmpty 2
        
        n1.equalTo emptyNode |> should equal false
        n2.equalTo n1 |> should equal false
        n2.equalTo emptyNode |> should equal false
        n2.Content.Board |> should haveLength 81
        
        let moreNodes = n2.getChildren
        n2.equalTo emptyNode |> should equal false
        moreNodes.Head.Content.Board |> should haveLength 81
        n2.equalTo moreNodes.Head |> should equal false
        
        let n3 = n2.getChildren.Head
        n3.equalTo n2 |> should equal false

    [<Test>]
    member test.
        ``Children from a node should have one more selected number``() =
        let emptyNode = new SudokuNode(new SudokuProblemComplete(emptyBoard))
        let hm = List.filter(fun x -> x > 0) emptyNode.Content.Board
        hm |> should haveLength 0

        let childOfEmpty = emptyNode.getChildren.Head
        let hm2 = List.filter(fun x -> x > 0) childOfEmpty.Content.Board
        hm2 |> should haveLength 1
        childOfEmpty.Content.Board |> should haveLength 81

    [<Test>]
    member test.
        ``Two boards with same board should be equal``() =

        let notGoalNode =  new SudokuNode(new SudokuProblemComplete(board))
        let notGoalNode2 =  new SudokuNode(new SudokuProblemComplete(board))
        let fakeGoalNode = new SudokuNode(new SudokuProblemComplete(fakeGoal))

        (notGoalNode.equalTo notGoalNode2) |> should equal true
        (notGoalNode.equalTo fakeGoalNode) |> should equal false
        (fakeGoalNode.equalTo notGoalNode) |> should equal false


    [<Test>]
    member test.
        ``Should print the current state of the node``() =
        
        let notGoalNode =  new SudokuNode(new SudokuProblemComplete(board))
        let fakeGoalNode = new SudokuNode(new SudokuProblemComplete(fakeGoal))

        notGoalNode.print
        fakeGoalNode.print

        1 |> should equal 1

//
//
//
//
//
//
//
//
//
//
//
//
//
//
//
//[<TestFixture>]
//type NodeFixture() =
//    let nStart = new Node(new SudokuProblem(1, 0, 0, 0, 0, 0, 0, 0, 0) )
//    let nGoal = new Node(new SudokuProblem(1, 2, 3, 4, 5, 6, 7, 8, 9) )
//    let valueInMiddleNode = new Node(new SudokuProblem(0, 0, 0, 0, 5, 0, 0, 0, 0) )
//    let threeValuesNode = new Node(new SudokuProblem(0, 4, 0, 0, 5, 0, 3, 0, 0) )
//    let almostGoalNode = new Node(new SudokuProblem(1, 4, 2, 6, 5, 7, 3, 9, 0) )
//
//    [<Test>]
//    member test.
//        ``Based on known states of the Node isGoal should be true``() =
//
//       nStart.isGoal |> should equal false
//       nGoal.isGoal |> should equal true
//
//    [<Test>]
//    member test.
//        ``Should create 8 child nodes based on a node with one value``() =
//        
//        let children = nStart.getChildren
//        let randomChildren = valueInMiddleNode.getChildren
//
//        children |> should haveLength 8
//        randomChildren |> should haveLength 8
//
//    [<Test>]
//    member test.
//        ``Should create some child nodes based on a node with some values``() =
//        
//        let someChildren = threeValuesNode.getChildren
//        someChildren |> should haveLength 6
//
//    [<Test>]
//    member test.
//        ``Should create one child node when all but one slots are filled``() =
//        
//        let oneChild = almostGoalNode.getChildren
//        oneChild |> should haveLength 1
//
//    [<Test>]
//    member test.
//        ``Should create goal child node when all but one slots are filled``() =
//        
//        let goalChild = almostGoalNode.getChildren
//        goalChild.Head.isGoal |> should equal true
//    
//    [<Test>]
//    member test.
//        ``Two nodes should only be equal if they have the same values in the same spots``() =
//        
//        let first = new Node(new SudokuProblem(1, 0, 0, 0, 0, 0, 0, 0, 0) )
//        let second = new Node(new SudokuProblem(1, 0, 0, 0, 0, 0, 0, 0, 0) )
//        first.equalTo second |> should equal true
//        first.equalTo first |> should equal true
//
//        let first = new Node(new SudokuProblem(1, 2, 3, 4, 5, 6, 7, 0, 0) )
//        let second = new Node(new SudokuProblem(1, 0, 0, 0, 0, 0, 0, 0, 0) )
//        first.equalTo second |> should equal false
//
//        let first = new Node(new SudokuProblem(0, 1, 0, 0, 0, 0, 0, 0, 0) )
//        let second = new Node(new SudokuProblem(1, 0, 0, 0, 0, 0, 0, 0, 0) )
//        first.equalTo second |> should equal false
//    
//
//
//    [<Test>]
//    member test.
//        ``isGoal should only validate valid goals``() =
//
//        let first = new Node(new SudokuProblem(1, 2, 3, 4, 5, 6, 7, 8, 9) )
//        let second = new Node(new SudokuProblem(1, 1, 1, 1, 1, 1, 1, 1, 1) )
//
//        first.isGoal |> should equal true
//        second.isGoal |> should equal false
//
////[<TestFixture>]
////type SudokuProblemFixture() =
//
//    [<Test>]
//    member test.
//        ``Should Find Spots For Each Slot Of The List based on the previous step``() =
//        let step1 = new SudokuProblem(1, 0, 0, 0, 0, 0, 0, 0, 0)
//        let step2 = SudokuProblem.FromList (step1.findSpotFor 2)
//        let step3 = SudokuProblem.FromList (step2.findSpotFor 3)
//        let step4 = SudokuProblem.FromList (step3.findSpotFor 4)
//        let step5 = SudokuProblem.FromList (step4.findSpotFor 5)
//        let step6 = SudokuProblem.FromList (step5.findSpotFor 6)
//        let step7 = SudokuProblem.FromList (step6.findSpotFor 7)
//        let step8 = SudokuProblem.FromList (step7.findSpotFor 8)
//        let step9 = SudokuProblem.FromList (step8.findSpotFor 9)
//
//        step1.toList |> should equal [1; 0; 0; 0; 0; 0; 0; 0; 0]
//        step2.toList |> should equal [1; 2; 0; 0; 0; 0; 0; 0; 0]
//        step3.toList |> should equal [1; 2; 3; 0; 0; 0; 0; 0; 0]
//        step4.toList |> should equal [1; 2; 3; 4; 0; 0; 0; 0; 0]
//        step5.toList |> should equal [1; 2; 3; 4; 5; 0; 0; 0; 0]
//        step6.toList |> should equal [1; 2; 3; 4; 5; 6; 0; 0; 0]
//        step7.toList |> should equal [1; 2; 3; 4; 5; 6; 7; 0; 0]
//        step8.toList |> should equal [1; 2; 3; 4; 5; 6; 7; 8; 0]
//        step9.toList |> should equal [1; 2; 3; 4; 5; 6; 7; 8; 9]
//
//
//    [<Test>]
//    member test.
//        ``Should find spots on the list when there is one spot taken in the middle of the list``() = 
//        let step = new SudokuProblem(0, 0, 0, 0, 5, 0, 0, 0, 0)
//        let step2 = SudokuProblem.FromList (step.findSpotFor 1)
//        step2.toList |> should equal [1; 0; 0; 0; 5; 0; 0; 0; 0]
//
//    [<Test>]
//    member test.
//        ``Should find spots for a number that already exists (Should not care about the number itself)``() = 
//        let step = new SudokuProblem(0, 0, 0, 0, 5, 0, 0, 0, 0)
//        let step2 = SudokuProblem.FromList (step.findSpotFor 5)
//
//        step2.toList |> should equal [5; 0; 0; 0; 5; 0; 0; 0; 0]
//
//    [<Test>]
//    member test.
//        ``Should find a spot for a number when there are no numbers in the list``() = 
//        let step = new SudokuProblem(0, 0, 0, 0, 0, 0, 0, 0, 0)
//        let step2 = SudokuProblem.FromList (step.findSpotFor 5)
//
//        step2.toList |> should equal [5; 0; 0; 0; 0; 0; 0; 0; 0]
//
//    [<Test>]
//    member test.
//        ``Should create empty list when trying to find a slot for a number when the list is full``() = 
//        let step = new SudokuProblem(5, 5, 5, 5, 5, 5, 5, 5, 5)
//        let step2 = (step.findSpotFor 5)
//
//        match step2 with
//        | None -> "Success" |> should equal "Success"
//        | Some(a) -> failwith "There should not have been any results from this"
//
//    [<Test>]
//    member test.
//        ``Should throw exception when trying to create a SudokuProblem from an empty list``() = 
//        let step = new SudokuProblem(5, 5, 5, 5, 5, 5, 5, 5, 5)
//        let step2 = (step.findSpotFor 5)        
//        
//        (fun () ->
//            SudokuProblem.FromList step2 |> ignore )
//            |> should throw typeof<System.ArgumentException>

