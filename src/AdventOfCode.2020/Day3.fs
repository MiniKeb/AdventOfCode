module Day3

open Common

type Slope = {
    right: int;
    down: int;
};

let hasTree (slope:Slope) index (line:string) = 
    if index % slope.down = 1 then
        false
    else
        let position = (index * slope.right) % line.Length
        match (line.Chars position) with
            | '#' -> true
            | '.' -> false
            | c -> failwith $"Unexpected character '{c}'"
  
let goSlope (inputs:seq<string>) (slope:Slope)  = 
    inputs
        |> Seq.mapi (hasTree slope)
        |> Seq.where id
        |> Seq.length

let getFirstStar (inputs:seq<string>) = 
    let slope = { right = 3; down = 1; }
    goSlope inputs slope

let getSecondStar inputs = 
    [
        { right = 1; down = 1; };
        { right = 3; down = 1; };
        { right = 5; down = 1; };
        { right = 7; down = 1; };
        { right = 1; down = 2; };
    ] 
        |> Seq.map (goSlope inputs)
        |> Seq.fold (fun multiplication curr -> multiplication * uint curr) 1u


let run = 
    let inputs = readLines "C:\Development\AdventOfCode\src\AdventOfCode.2020\Input3.txt" |> Seq.toList
    printfn "*** DAY 3 ***"

    getFirstStar inputs |> (printfn "*\t: %i")
    getSecondStar inputs |> (printfn "**\t: %i")