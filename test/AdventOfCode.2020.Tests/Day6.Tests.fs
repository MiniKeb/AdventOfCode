module Tests.Day6

open NUnit.Framework
open NFluent
open Day6

[<SetUp>]
let Setup () =
    ()

[<Test>]
let OnePersonWithThreeAnswersShouldGiveAnyone3 () =
    let inputs = [ "abc" ]
    
    let awswerCount = getFirstStar inputs

    Check.That(awswerCount).IsEqualTo(3) |> ignore

    
[<Test>]
let TwoPersonsWithDifferentAnswerShouldGiveAnyone2 () =
    let inputs = [ "a"; "b" ]
        
    let awswerCount = getFirstStar inputs
    
    Check.That(awswerCount).IsEqualTo(2) |> ignore


[<Test>]
let TwoPersonsWithSameAnswerShouldGiveAnyone1 () =
    let inputs = [ "a"; "a" ]
        
    let awswerCount = getFirstStar inputs
    
    Check.That(awswerCount).IsEqualTo(1) |> ignore

[<Test>]
let TwoPersonsWithOneSameAnswerAndTwoDifferentsAnswersShouldGiveAnyone3 () =
    let inputs = [ "ab"; "ac" ]
        
    let awswerCount = getFirstStar inputs
    
    Check.That(awswerCount).IsEqualTo(3) |> ignore

[<Test>]
let TwoGroupsWithScore3ShouldGiveAnyone6 () =
    let inputs = [ "abc"; ""; "a"; "b"; "c" ]
        
    let awswerCount = getFirstStar inputs
    
    Check.That(awswerCount).IsEqualTo(6) |> ignore


[<Test>]
let OnePersonWithThreeAnswersShouldGiveEveryone3 () =
    let inputs = [ "abc" ]
    
    let awswerCount = getSecondStar inputs

    Check.That(awswerCount).IsEqualTo(3) |> ignore

    
[<Test>]
let TwoPersonsWithDifferentAnswerShouldGiveEveryone0 () =
    let inputs = [ "a"; "b" ]
        
    let awswerCount = getSecondStar inputs
    
    Check.That(awswerCount).IsEqualTo(0) |> ignore


[<Test>]
let TwoPersonsWithSameAnswerShouldGiveEveryone1 () =
    let inputs = [ "a"; "a" ]
        
    let awswerCount = getSecondStar inputs
    
    Check.That(awswerCount).IsEqualTo(1) |> ignore

[<Test>]
let TwoPersonsWithOneSameAnswerAndTwoDifferentsAnswersShouldGiveEveryone1 () =
    let inputs = [ "ab"; "ac" ]
        
    let awswerCount = getSecondStar inputs
    
    Check.That(awswerCount).IsEqualTo(1) |> ignore

[<Test>]
let TwoGroupsWithScore3ShouldGiveEveryone6 () =
    let inputs = [ "abc"; ""; "ac"; "ab"; "bac" ]
        
    let awswerCount = getSecondStar inputs
    
    Check.That(awswerCount).IsEqualTo(4) |> ignore