// https://adventofcode.com/2021/day/1

open System.IO

let input = File.ReadAllLines(Path.Join(__SOURCE_DIRECTORY__, "input.txt"))

input
|> Seq.map int
|> Seq.pairwise
|> Seq.filter (fun (x,y) -> x < y) // use pattern matching deconstruction
|> Seq.length