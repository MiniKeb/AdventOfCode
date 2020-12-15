module Day4

open Common
open System.Text.RegularExpressions

type Passport = {
    BirthYear: string;
    IssueYear: string;
    ExpirationYear: string;
    Height: string;
    HairColor: string;
    EyeColor: string;
    PassportId: string;
    CountryId: string;
}
let emptyPassport = { 
    BirthYear = null;
    IssueYear = null;
    ExpirationYear = null;
    Height = null;
    HairColor= null;
    EyeColor = null;
    PassportId = null;
    CountryId= null;
}

let isValidPassport (passport:Passport) = 
    passport.BirthYear <> null
    && passport.IssueYear <> null
    && passport.ExpirationYear <> null
    && passport.Height <> null
    && passport.HairColor <> null
    && passport.EyeColor <> null
    && passport.PassportId <> null


let isValidPassportBis (passport:Passport) = 
    let (<=>) (numText:string) (min, max) = 
        let num = int numText
        min <= num && num <= max

    let (<^>) text regex = 
        Regex.Match(text, regex).Success

    let isValidEyeColor ecl = 
        match ecl with 
            | "amb"
            | "blu" 
            | "brn" 
            | "gry" 
            | "grn" 
            | "hzl" 
            | "oth" -> true
            | _ -> false

    let isValidHeight text = 
        let isValid size unit = 
            match unit with
                | "cm" -> size <=> (150, 193)
                | "in" -> size <=> (59, 76)
                | _ -> false

        let m = Regex.Match(text, "^(?<size>\d{2,3})(?<unit>cm|in)$")
        m.Success && (isValid m.Groups.["size"].Value m.Groups.["unit"].Value)

    (isValidPassport passport)
    && passport.BirthYear <=> (1920, 2002)
    && passport.IssueYear <=> (2010,2020)
    && passport.ExpirationYear <=> (2020, 2030)
    && isValidHeight passport.Height
    && passport.HairColor <^> "^#([0-9]|[a-f]){6}"
    && isValidEyeColor passport.EyeColor
    && passport.PassportId <^> "\d{9}" && passport.PassportId.Length = 9

let linkPassportCodes passport (key, value) = 
    match key with
        | "byr" -> {passport with BirthYear = value}
        | "iyr" -> {passport with IssueYear = value}
        | "eyr" -> {passport with ExpirationYear = value}
        | "hgt" -> {passport with Height = value}
        | "hcl" -> {passport with HairColor = value}
        | "ecl" -> {passport with EyeColor = value}
        | "pid" -> {passport with PassportId = value}
        | "cid" -> {passport with CountryId = value}
        | _ -> failwith $"Unknown value : {value}"

let linkCode (passports:list<Passport>) keyValuePair = 
    match keyValuePair with
        | Some pair -> 
            match passports with
            | head :: tail -> 
                let passport = linkPassportCodes head pair
                passport :: tail
            | _ -> failwith $"I dunno what I do"
        | None -> emptyPassport :: passports

let buildLineMap (line:string) = 
    let collection = Regex.Matches(line, "(?<key>\w{3}):(?<value>(#|\w|\d)+)")
    if collection.Count > 0 then 
        collection
            |> Seq.map (fun m -> Some (m.Groups.["key"].Value, m.Groups.["value"].Value))
    else
        seq{ None }

let buildPassports (lines:seq<string>) = 
    lines 
        |> Seq.collect buildLineMap
        |> Seq.fold linkCode [ emptyPassport ]

let getFirstStar (inputs:seq<string>) = 
    inputs 
        |> buildPassports
        |> Seq.filter isValidPassport
        |> Seq.length

let getSecondStar (inputs:seq<string>) = 
    inputs 
        |> buildPassports
        |> Seq.filter isValidPassportBis
        |> Seq.length

let run = 
    let inputs = readLines "C:\Development\AdventOfCode\src\AdventOfCode.2020\Input4.txt" |> Seq.toList
    printfn "*** DAY 4 ***"

    getFirstStar inputs |> (printfn "*\t: %i")
    getSecondStar inputs |> (printfn "**\t: %i")