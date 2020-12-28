module Day6

open Common

type PeopleAnswers = Set<char>

type GroupAnswers = PeopleAnswers list

type CountOfAGroup = GroupAnswers -> int

let buildPeopleAnswers (line:string) =
    let answers = line.ToCharArray() |> Set.ofArray
    PeopleAnswers answers

let foldIntoGroupAnswers (groups:GroupAnswers list) (line:string) = 
    if line.Length <= 0 then
        let newGroup: GroupAnswers = list.Empty
        newGroup :: groups
    else
        let peopleAnswers = buildPeopleAnswers line
        match groups with
        | group :: rest -> 
            let currentGroup = (peopleAnswers :: group)
            currentGroup :: rest
        | [] -> [ [peopleAnswers] ]


let anyoneOfAGroup (group:GroupAnswers) = 
    group 
        |> Seq.fold (fun acc cur -> Set.union acc cur) Set.empty
        |> Seq.length

let everyoneOfAGroup (group:GroupAnswers) = 
    match group with 
    | head :: _ -> 
        group 
            |> Seq.fold (fun acc cur -> Set.intersect acc cur) head
            |> Seq.length
    | [] -> 0

let calculate (method:CountOfAGroup) (inputs:seq<string>) =
    inputs
        |> Seq.fold foldIntoGroupAnswers list.Empty
        |> Seq.map method
        |> Seq.sum

let getFirstStar = calculate anyoneOfAGroup

let getSecondStar = calculate everyoneOfAGroup

let run = 
    let inputs = readLines "C:\Development\AdventOfCode\src\AdventOfCode.2020\Input6.txt" |> Seq.toList
    printfn "*** DAY 6 ***"

    getFirstStar inputs |> (printfn "*\t: %i")
    getSecondStar inputs |> (printfn "**\t: %i")

