module Common

open System.IO

let readLines (filePath:string) = seq {
    use streamReader = new StreamReader (filePath)
    while not streamReader.EndOfStream do
        yield streamReader.ReadLine()
}

