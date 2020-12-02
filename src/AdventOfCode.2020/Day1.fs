module Day1

open Common

let firstStar expenses target = 
    let existsInExpenses item = expenses |> Seq.contains item
    
    expenses 
        |> Seq.map (fun item -> 
            let rest = target - item
            (item, rest), (existsInExpenses rest, item * rest))
        |> Seq.where (snd >> fst)

let getFirstStar expenses target = 
    firstStar expenses target
        |> Seq.head
        |> (snd >> snd)

let getSecondStar expenses = 
    let existsBiggerExpensesThan item = expenses |> Seq.exists (fun i -> i > item)
    
    expenses
        |> Seq.map (fun n -> 
            let objective = 2020 - n
            (n, objective), existsBiggerExpensesThan objective)
        |> Seq.where snd
        |> Seq.map fst
        |> Seq.collect (fun (a, o) -> (firstStar expenses o) |> Seq.map (fun ((x,y),_) -> (a, x, y, a * x * y)) )
        |> Seq.head

let run = 
    let inputs = readLines "C:\Development\AdventOfCode\src\AdventOfCode.2020\Input1.txt" |> Seq.map int |> Seq.toList
    printfn "*** DAY 1 ***"
    
    getFirstStar inputs 2020 |> (printfn "*\t: %i")
    
    let fourth (_,_,_,r) = r
    getSecondStar inputs |> fourth |> (printfn "**\t: %i")
                            
