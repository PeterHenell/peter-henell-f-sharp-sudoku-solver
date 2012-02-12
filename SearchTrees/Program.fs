module Program
open DataStructure2
open Solver


 //We are throwing different kind of exceptions to handle failure and success.
 //Todo: Do not use exceptions lol

let easy = [
    3;0;9;7;0;5;8;0;1;
    0;0;2;0;0;0;5;0;0;
    0;1;0;0;0;0;0;6;0;
    0;0;5;4;0;3;2;0;0;
    6;0;0;0;0;0;0;0;7;
    0;0;3;8;0;6;1;0;0;
    0;4;0;0;0;0;0;2;0;
    0;0;7;0;0;0;3;0;0;
    2;0;8;1;0;9;6;0;4;
]


let s = new Solver()
try
    let res = s.solve (new SudokuNode(new SudokuProblemComplete(easy) ))
    res |> ignore
with
    | :? NoSolutionFound as notFound -> printfn "%O" notFound.Data0
    | :? SolutionWasFound as found -> printfn "%O" (found.ToString())

printfn "ok, next"

let hard = [
    0;0;5;0;3;4;0;0;0;
    0;0;6;7;0;0;3;0;0;
    0;0;0;9;0;0;0;7;2;
    0;0;2;8;0;0;5;0;3;
    0;1;0;0;0;0;0;4;0;
    8;0;4;0;0;3;2;0;0;
    2;5;0;0;0;8;0;0;0;
    0;0;7;0;0;6;1;0;0;
    0;0;0;4;9;0;7;0;0;
]

try
    let res = s.solve (new SudokuNode(new SudokuProblemComplete(hard) ))
    res |> ignore
with
    | :? NoSolutionFound as notFound -> printfn "%O" notFound.Data0
    | :? SolutionWasFound as found -> printfn "%O" (found.ToString())

System.Console.ReadKey() |> ignore
printfn "ok, all done"