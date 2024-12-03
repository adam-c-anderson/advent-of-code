// https://adventofcode.com/2024/day/2

open System.IO
open System.Text.RegularExpressions

let sample = File.ReadAllLines(Path.Join(__SOURCE_DIRECTORY__, "sample.txt"))
let input = File.ReadAllLines(Path.Join(__SOURCE_DIRECTORY__, "input.txt"))

let parseLine (line:string) =
    let m = Regex.Match(line, @"(?:(\d+)\s*)+")
    m.Groups[1].Captures
    |> Seq.map (_.Value >> int)
    |> Seq.toList

module Report =
    let isSafe report =
        let pairs = report |> List.pairwise
        pairs |> List.forall (fun (a, b) -> (a - b) >= 1 && (a - b) <= 3) ||
        pairs |> List.forall (fun (a, b) -> (a - b) <= -1 && (a - b) >= -3)

    let dampen report = seq {
        for i in 0 .. (List.length report) - 1 ->
            List.removeAt i report
    }

    let isSafeDampened report = report |> dampen |> Seq.exists isSafe

let part1 input =
    input
    |> Array.map (parseLine >> Report.isSafe)
    |> Array.filter id
    |> Array.length

let part2 input =
    input
    |> Array.map parseLine
    |> Array.map (fun r -> Report.isSafe r || Report.isSafeDampened r)
    |> Array.filter id
    |> Array.length

sample |> part1
input |> part1
sample |> part2
input |> part2