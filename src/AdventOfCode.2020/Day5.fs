module Day5


open Common

type Seat = {
    Row: int;
    Column: int;
}

let calculateSeatId (seat:Seat) =
    seat.Row * 8 + seat.Column

type Zone (bottom, up) = 
    let getMiddle = (decimal bottom + (decimal up - decimal bottom) / 2m)
    member this.Bottom = bottom
    member this.Up = up
    member this.UpperHalf = 
        let startHalf = getMiddle |> ceil |> int
        new Zone(startHalf, up)
    member this.LowerHalf = 
        let endHalf = getMiddle |> floor |> int
        new Zone(bottom, endHalf)
    override this.Equals(obj) = 
        match obj with
            | :? Zone as z -> (bottom, up) = (z.Bottom, z.Up)
            | _ -> false


let searchRow (rowZone:Zone, columnZone:Zone) directionChar = 
    match directionChar with 
    | 'B' -> (rowZone.UpperHalf, columnZone)
    | 'F' -> (rowZone.LowerHalf, columnZone)
    | 'R' -> (rowZone, columnZone.UpperHalf)
    | 'L' -> (rowZone, columnZone.LowerHalf)
    | _ -> failwith $"Unsupported direction '{directionChar}'"

let getSeat (boardCode:string) = 
    boardCode.ToCharArray()
        |> Seq.fold searchRow (new Zone(0, 127), new Zone(0, 7))
        |> fun (row, column) -> { Row = row.Up; Column = column.Up }

let getFirstStar inputs = 
    inputs 
    |> Seq.map (getSeat >> calculateSeatId)
    |> Seq.max

let getSecondStar inputs = 
    let seatIds = inputs 
                    |> Seq.map (getSeat >> calculateSeatId)
                    |> Seq.sort
                    |> Set.ofSeq

    let minSeat = seatIds |> Seq.min
    let maxSeat = seatIds |> Seq.max

    let all = [| minSeat..maxSeat |] |> Set.ofArray

    Set.difference all seatIds
        |> Seq.exactlyOne

let run = 
    let inputs = readLines "C:\Development\AdventOfCode\src\AdventOfCode.2020\Input5.txt" |> Seq.toList
    printfn "*** DAY 5 ***"

    getFirstStar inputs |> (printfn "*\t: %i")
    getSecondStar inputs |> (printfn "**\t: %i")