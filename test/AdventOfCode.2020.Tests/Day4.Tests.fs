module Tests.Day4

open NUnit.Framework
open NFluent
open Day4

[<SetUp>]
let Setup () =
    ()

[<Test>]
let ShouldPassSample () =
    let inputs = "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd
byr:1937 iyr:2017 cid:147 hgt:183cm
    
iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884
hcl:#cfa07d byr:1929
    
hcl:#ae17e1 iyr:2013
eyr:2024
ecl:brn pid:760753108 byr:1931
hgt:179cm
    
hcl:#cfa07d eyr:2025 pid:166559648
iyr:2011 ecl:brn hgt:59in";
    
    let validCount = getFirstStar (inputs.Split("\n"))

    Check.That(validCount).IsEqualTo(2) |> ignore


[<Test>]
let ShouldntPass2ndSample () =
    let inputs = "eyr:1972 cid:100
hcl:#18171d ecl:amb hgt:170 pid:186cm iyr:2018 byr:1926
    
iyr:2019
hcl:#602927 eyr:1967 hgt:170cm
ecl:grn pid:012533040 byr:1946
    
hcl:dab227 iyr:2012
ecl:brn hgt:182cm pid:021572410 eyr:2020 byr:1992 cid:277
    
hgt:59cm ecl:zzz
eyr:2038 hcl:74454a iyr:2023
pid:3556412378 byr:2007";
    
    let validCount = getSecondStar (inputs.Split("\n"))

    Check.That(validCount).IsEqualTo(0) |> ignore

[<Test>]
let ShouldPass3ndSample () =
    let inputs = "pid:087499704 hgt:59in ecl:grn iyr:2010 eyr:2030 byr:1920
    hcl:#623a2f
    
    eyr:2020 ecl:blu cid:129 byr:2002
    iyr:2020 pid:896056539 hcl:#a97842 hgt:193cm
    
    hcl:#888785
    hgt:164cm byr:2001 iyr:2015 cid:88
    pid:545766238 ecl:hzl
    eyr:2022
    
    iyr:2010 hgt:158cm hcl:#b6652a ecl:blu byr:1944 eyr:2021 pid:093154719";
    
    let validCount = getSecondStar (inputs.Split("\n"))

    Check.That(validCount).IsEqualTo(4) |> ignore


[<Test>]
let ShouldParseOneLine () =
    let inputs = "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd";
    
    let map = buildLineMap inputs

    Check.That(map).ContainsExactly(
        Some("ecl", "gry"), 
        Some("pid", "860033327"), 
        Some("eyr", "2020"), 
        Some("hcl", "#fffffd")) |> ignore

[<Test>]
let ShouldParseTwoLine () =
    let inputs = [| "ecl:gry pid:860033327"; "eyr:2020 hcl:#fffffd" |]
    
    let passports = buildPassports inputs

    Check.That(List.exactlyOne passports).IsEqualTo(
        { 
            BirthYear = null;
            IssueYear = null;
            ExpirationYear = "2020";
            Height = null;
            HairColor= "#fffffd";
            EyeColor = "gry";
            PassportId = "860033327";
            CountryId= null;
        }) |> ignore


[<Test>]
let ShouldParseTwoPassport () =
    let inputs = [| "ecl:gry pid:860033327"; ""; "eyr:2020 hcl:#fffffd" |]
    
    let passports = buildPassports inputs

    Check.That(passports.Length).IsEqualTo(2) |> ignore
    Check.That(List.toSeq passports).ContainsExactly(
        { 
            BirthYear = null;
            IssueYear = null;
            ExpirationYear = "2020";
            Height = null;
            HairColor= "#fffffd"
            EyeColor = null;
            PassportId = null;
            CountryId= null;
        },
        { 
            BirthYear = null;
            IssueYear = null;
            ExpirationYear = null;
            HairColor= null;
            Height = null;
            EyeColor = "gry";
            PassportId = "860033327";
            CountryId= null;
        }) |> ignore


