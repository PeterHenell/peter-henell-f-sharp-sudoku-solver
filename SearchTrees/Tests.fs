module NodeTests

open NUnit.Framework
open FsUnit
open DataStructure2

[<TestFixture>]
type SudokuNodeFixture() =
    let board = [|   
                    0; 0; 8; 0; 1; 0; 0; 0; 9;
                    6; 0; 1; 0; 9; 0; 2; 3; 0;
                    0; 4; 0; 0; 3; 7; 0; 0; 5;
                    0; 3; 5; 0; 0; 8; 2; 0; 0;
                    0; 0; 2; 6; 5; 0; 8; 0; 0;
                    0; 0; 4; 0; 0; 1; 7; 5; 0;
                    5; 0; 0; 3; 4; 0; 0; 8; 0;
                    0; 9; 7; 0; 8; 0; 5; 0; 6;
                    1; 0; 0; 0; 6; 0; 9; 0; 0; |]
    let board2 = [|   
                    2; 5; 8; 0; 1; 0; 0; 0; 9;
                    6; 0; 1; 0; 9; 0; 2; 3; 0;
                    0; 4; 0; 0; 3; 7; 0; 0; 5;
                    0; 3; 5; 0; 0; 8; 2; 0; 0;
                    0; 0; 2; 6; 5; 0; 8; 0; 0;
                    0; 0; 4; 0; 0; 1; 7; 5; 0;
                    5; 0; 0; 3; 4; 0; 0; 8; 0;
                    0; 9; 7; 0; 8; 0; 5; 0; 6;
                    1; 0; 0; 0; 6; 0; 9; 0; 0; |]
    let fakeGoal = [|   
                    2; 5; 8; 4; 1; 4; 4; 4; 9;
                    6; 4; 1; 4; 9; 5; 2; 3; 4;
                    5; 4; 5; 5; 3; 7; 5; 5; 5;
                    5; 3; 5; 5; 5; 8; 2; 5; 5;
                    5; 5; 2; 6; 5; 5; 8; 5; 5;
                    5; 5; 4; 5; 5; 1; 7; 5; 5;
                    5; 5; 5; 3; 4; 5; 5; 8; 5;
                    5; 9; 7; 5; 8; 5; 5; 5; 6;
                    1; 5; 5; 5; 6; 5; 9; 5; 5; |]
    
    let emptyBoard = [|
                    0; 0; 0; 0; 0; 0; 0; 0; 0;
                    0; 0; 0; 0; 0; 0; 0; 0; 0;
                    0; 0; 0; 0; 0; 0; 0; 0; 0;
                    0; 0; 0; 0; 0; 0; 0; 0; 0;
                    0; 0; 0; 0; 0; 0; 0; 0; 0;
                    0; 0; 0; 0; 0; 0; 0; 0; 0;
                    0; 0; 0; 0; 0; 0; 0; 0; 0;
                    0; 0; 0; 0; 0; 0; 0; 0; 0;
                    0; 0; 0; 0; 0; 0; 0; 0; 0; |]

    let problem = new SudokuProblemComplete(board)
    let problem2 = new SudokuProblemComplete(board2)


    [<Test>]
    member test.
        ``Should get the list that corresponds to a column``() =
        let a = problem.getCol 1
        let b = problem.getCol 9

        a |> should equal [|0;
                           6;
                           0;
                           0;
                           0;
                           0;
                           5;
                           0;
                           1; |]
        
        b |> should equal [|9;
                           0; 
                           5;
                           0;
                           0;
                           0;
                           0;
                           6;
                           0; |]
    [<Test>]
    member test.
        ``Should get the list that corresponds to a row``() =
        let a = problem.getRow 1
        let b = problem.getRow 9

        a |> should equal [|0; 0; 8; 0; 1; 0; 0; 0; 9; |]
        b |> should equal [|1; 0; 0; 0; 6; 0; 9; 0; 0; |]
    
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

        a |> should equal [| 0; 0; 8;
                            6; 0; 1;
                            0; 4; 0; |]


        a3 |> should equal [|
                            0; 0; 9; 
                            2; 3; 0;
                            0; 0; 5;|]


        a4 |> should equal [|
                            0; 3; 5;
                            0; 0; 2;
                            0; 0; 4; |]

        a5 |> should equal [|
                          0; 0; 8;
                          6; 5; 0;
                          0; 0; 1; |]

        a6 |> should equal [|
                            2; 0; 0;
                            8; 0; 0;
                            7; 5; 0; |]

        a7 |> should equal [|
                        5; 0; 0; 
                        0; 9; 7;
                        1; 0; 0;|]

        a8 |> should equal [|
                        3; 4; 0;
                        0; 8; 0;
                        0; 6; 0; |]

        a9 |> should equal [|
                           0; 8; 0;
                           5; 0; 6;
                           9; 0; 0; |]




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



        let n1 = childOfEmpty.[ 1 ]
        let n2 = childOfEmpty.[ 2 ]
        
        n1.equalTo emptyNode |> should equal false
        n2.equalTo n1 |> should equal false
        n2.equalTo emptyNode |> should equal false
        n2.Content.Board |> should haveLength 81
        
        let moreNodes = n2.getChildren
        n2.equalTo emptyNode |> should equal false
        moreNodes.[0].Content.Board |> should haveLength 81
        n2.equalTo moreNodes.[0] |> should equal false
        
        let n3 = n2.getChildren.[0]
        n3.equalTo n2 |> should equal false

    [<Test>]
    member test.
        ``Children from a node should have one more selected number``() =
        let emptyNode = new SudokuNode(new SudokuProblemComplete(emptyBoard))
        let hm = Array.filter(fun x -> x > 0) emptyNode.Content.Board
        hm |> should haveLength 0

        let childOfEmpty = emptyNode.getChildren.[0]
        let hm2 = Array.filter(fun x -> x > 0) childOfEmpty.Content.Board
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
