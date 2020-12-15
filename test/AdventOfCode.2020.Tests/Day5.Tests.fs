module Tests.Day5

open NUnit.Framework
open NFluent
open Day5

[<SetUp>]
let Setup () =
    ()

[<Test>]
let FirstStarShouldReturn820 () =
    let inputs = [ "BFFFBBFRRR"; "BBFFBBFRLL"; "FFFBBBFRRR" ]
    
    let maxSeatId = getFirstStar inputs

    Check.That(maxSeatId).IsEqualTo(820) |> ignore


[<Test>]
let ShouldBeRow70Column7 () =
    let input = "BFFFBBFRRR";
    
    let seat = getSeat input

    Check.That(seat).IsEqualTo({Row = 70; Column = 7}) |> ignore
    Check.That(calculateSeatId seat).IsEqualTo(567) |> ignore


[<Test>]
let ShouldBeRow14Column7 () =
    let input = "FFFBBBFRRR";
    
    let seat = getSeat input

    Check.That(seat).IsEqualTo({Row = 14; Column = 7}) |> ignore
    Check.That(calculateSeatId seat).IsEqualTo(119) |> ignore

[<Test>]
let ShouldBeRow102Column4 () =
    let input = "BBFFBBFRLL";
    
    let seat = getSeat input

    Check.That(seat).IsEqualTo({Row = 102; Column = 4}) |> ignore
    Check.That(calculateSeatId seat).IsEqualTo(820) |> ignore


[<Test>]
let ShouldBeRow127Column0 () =
    let input = "FBFBFBFLRL";
    
    let seat = getSeat input

    Check.That(seat).IsEqualTo({Row = 42; Column = 2}) |> ignore
    Check.That(calculateSeatId seat).IsEqualTo(338) |> ignore

[<Test>]
let ZoneShouldProvideUpperAndLowerHalf () =
    let zone = new Zone(0, 127)

    Check.That(zone.LowerHalf).IsEqualTo(new Zone(0, 63)) |> ignore
    Check.That(zone.UpperHalf).IsEqualTo(new Zone(64, 127)) |> ignore
    