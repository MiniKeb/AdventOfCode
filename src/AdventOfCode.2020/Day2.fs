module Day2

open Common
open System.Text.RegularExpressions

type PasswordPolicy = {
    character : char;
    range : int * int;
};

type PasswordInfo = {
    policy : PasswordPolicy;
    password : string;
};

type Validator = PasswordInfo -> bool

let isInRange (info:PasswordInfo) = 
    let occurenceCount = Regex.Matches(info.password, string info.policy.character).Count
    (fst info.policy.range) <= occurenceCount && occurenceCount <= (snd info.policy.range)

let isInPosition (info:PasswordInfo) =
    (info.password.Chars (fst info.policy.range - 1) = info.policy.character) <> (info.password.Chars (snd info.policy.range - 1) = info.policy.character)

let buildPasswordInfo (textInfo:string) = 
    let (|Regex|_|) pattern input =
        let m = Regex.Match(input, pattern)
        if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
        else None

    match textInfo with 
        | Regex "^(\d+)-(\d+) (\w): (\w+)" [ startRange; endRange; letter; password; ]
            -> { policy = { character = char letter; range = (int startRange, int endRange) }; password = password }
        | _ -> failwith $"Unable to parse %s{textInfo}"

let getStar (validator:Validator) inputs = 
    inputs
        |> Seq.map buildPasswordInfo
        |> Seq.where validator
        |> Seq.length

let getFirstStar = getStar isInRange
let getSecondStar = getStar isInPosition

let run = 
    let inputs = readLines "C:\Development\AdventOfCode\src\AdventOfCode.2020\Input2.txt" |> Seq.toList
    printfn "*** DAY 2 ***"
    
    getFirstStar inputs |> (printfn "*\t: %i")
    getSecondStar inputs |> (printfn "**\t: %i")
                            