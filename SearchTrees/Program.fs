// Not yet sudoku, but something simular
// [list of 9 items]
// rules:
// every item in the list must be unique
// only numbers 1-9 are considered valid
// use 0 (zero) to indicate that a spot in the list have not been taken by any number

// Input to the function is an list where one of the fields is a number between 1 and 9
// The result should be a list with all nine slots filled with unique numbers


module Program
open DataStructure
open Solver


 //We are throwing different kind of exceptions to handle failure and success.
 //Todo: Do not use exceptions lol

let s = new Solver()
try
    let res = s.solve (new Node(new SudokuProblem(1, 0, 0, 0, 0, 0, 0, 0, 0) ))
    res |> ignore
with
    | :? NoSolutionFound -> printfn "Hittade inget"
    | :? SolutionWasFound as found -> printfn "%O" (found.ToString())




//let board = [   
//                    0; 0; 8; 0; 1; 0; 0; 0; 9;
//                    6; 0; 1; 0; 9; 0; 2; 3; 0;
//                    0; 4; 0; 0; 3; 7; 0; 0; 5;
//                    0; 3; 5; 0; 0; 8; 2; 0; 0;
//                    0; 0; 2; 6; 5; 0; 8; 0; 0;
//                    0; 0; 4; 0; 0; 1; 7; 5; 0;
//                    5; 0; 0; 3; 4; 0; 0; 8; 0;
//                    0; 9; 7; 0; 8; 0; 5; 0; 6;
//                    1; 0; 0; 0; 6; 0; 9; 0; 0; ]
