module AdventOfCode._2020.Tests

open NUnit.Framework
open NFluent
open Day3

[<SetUp>]
let Setup () =
    ()

let oneSquareSlope = { right = 1; down = 1; }

[<Test>]
let ShouldPassSample () =
    let inputs = [
        "..##.......";
        "#...#...#..";
        ".#....#..#.";
        "..#.#...#.#";
        ".#...##..#.";
        "..#.##.....";
        ".#.#.#....#";
        ".#........#";
        "#.##...#...";
        "#...##....#";
        ".#..#...#.#";
    ]

    let treeCount = goSlope inputs oneSquareSlope 
    
    Check.That(treeCount).IsEqualTo(2) |> ignore

[<Test>]
let ShouldPassAnotherSample () =
    let inputs = [
        "..##.";
        "#...#";
        ".#...";
        "..#.#";
        ".#...";
        "..#.#";
        ".#.#.";
        ".#...";
        "#.##.";
        "#...#";
        ".#..#";
    ]

    let treeCount = goSlope inputs oneSquareSlope 
    
    Check.That(treeCount).IsEqualTo(3) |> ignore


[<Test>]
let ShouldBeTrueWhenTreeExistsAtPosition () =
    let line = ".#..";
    
    let isItATree = hasTree oneSquareSlope 1 line

    Check.That(isItATree).IsTrue() |> ignore

[<Test>]
let ShouldBeFalseWhenTreeDoesntExistAtPosition () =
    let line = ".#..";
    
    let isItATree = hasTree oneSquareSlope 0 line

    Check.That(isItATree).IsFalse() |> ignore

[<Test>]
let ShouldThrowErrorWhenUnknownCharIsMet () =
    let line = "????";
    
    let toCheckTree = System.Func<bool> (fun () -> hasTree oneSquareSlope 3 line)

    // I dunno why but the .ThrowsAny() don't want to compile
    Check.ThatCode(toCheckTree).Not.DoesNotThrow() |> ignore

[<Test>]
let ShouldBeTrueWhenTreeExistsAtOverflowPosition () =
    let line = ".#..";
    
    let isItATree = hasTree oneSquareSlope 5 line

    Check.That(isItATree).IsTrue() |> ignore

[<Test>]
let ShouldBeFalseWhenTreeDoesntExistAtOverflowPosition () =
    let line = ".#..";
    
    let isItATree = hasTree oneSquareSlope 4 line

    Check.That(isItATree).IsFalse() |> ignore

