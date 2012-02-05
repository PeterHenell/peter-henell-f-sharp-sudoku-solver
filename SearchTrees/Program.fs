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
