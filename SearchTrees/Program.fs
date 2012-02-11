// Not yet sudoku, but something simular
// [list of 9 items]
// rules:
// every item in the list must be unique
// only numbers 1-9 are considered valid
// use 0 (zero) to indicate that a spot in the list have not been taken by any number

// Input to the function is an list where one of the fields is a number between 1 and 9
// The result should be a list with all nine slots filled with unique numbers


module Program
open DataStructure2
open Solver


 //We are throwing different kind of exceptions to handle failure and success.
 //Todo: Do not use exceptions lol

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

let s = new Solver()
try
    let res = s.solve (new SudokuNode(new SudokuProblemComplete(board) ))
    res |> ignore
with
    | :? NoSolutionFound as notFound -> printfn "%O" notFound
    | :? SolutionWasFound as found -> printfn "%O" (found.ToString())


let board2 = [0..80]
let printTests =
    for i in 0..80 do
        printf "%O ;" (List.nth board2 i )
        if (i + 1) % 9 = 0 then
            printfn ""
    printfn "" 

let c = 1
//let board = [   
//                    0; 0; 8; 0; 1; 0; 0; 0; 9; -- 0
//                    6; 0; 1; 0; 9; 0; 2; 3; 0; -- 9
//                    0; 4; 0; 0; 3; 7; 0; 0; 5; -- 18
//                    0; 3; 5; 0; 0; 8; 2; 0; 0; -- 27
//                    0; 0; 2; 6; 5; 0; 8; 0; 0; -- 36
//                    0; 0; 4; 0; 0; 1; 7; 5; 0; -- 45
//                    5; 0; 0; 3; 4; 0; 0; 8; 0; -- 54
//                    0; 9; 7; 0; 8; 0; 5; 0; 6; -- 63
//                    1; 0; 0; 0; 6; 0; 9; 0; 0; ] -- 72
let poss = [
    0  ;1  ;2  ;3  ;4  ;5  ;6  ;7  ;8  ;
    9  ;10 ;11 ;12 ;13 ;14 ;15 ;16 ;17 ;
    18 ;19 ;20 ;21 ;22 ;23 ;24 ;25 ;26 ;
    27 ;28 ;29 ;30 ;31 ;32 ;33 ;34 ;35 ;
    36 ;37 ;38 ;39 ;40 ;41 ;42 ;43 ;44 ;
    45 ;46 ;47 ;48 ;49 ;50 ;51 ;52 ;53 ;
    54 ;55 ;56 ;57 ;58 ;59 ;60 ;61 ;62 ;
    63 ;64 ;65 ;66 ;67 ;68 ;69 ;70 ;71 ;
    72 ;73 ;74 ;75 ;76 ;77 ;78 ;79 ;80 ; ]

// 1 = 0
// 2 = 3
// 3 = 6
// 4 = 18
// 5 = 21
// 6 = 24
// 7 = 54
// 8 = 57
// 9 = 60