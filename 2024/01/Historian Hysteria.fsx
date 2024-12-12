// https://adventofcode.com/2024/day/1

open System.IO
open System.Text.RegularExpressions

type Tuple =
    static member Map f (a, b) = f (a, b)

let sample = File.ReadAllLines(Path.Join(__SOURCE_DIRECTORY__, "sample.txt"))
let input = File.ReadAllLines(Path.Join(__SOURCE_DIRECTORY__, "input.txt"))

let parseLine (line:string) =
    let m = Regex.Match(line, @"(\d+)\s+(\d+)")
    int(m.Groups[1].Value),int(m.Groups[2].Value)

let part1 input =
    input
    |> Array.map parseLine
    |> Array.unzip
    |> Tuple.Map (fun (a, b) -> (Array.sort a),(Array.sort b))
    ||> Array.zip
    |> Array.map (fun (a, b) -> abs(a - b))
    |> Array.sum

let part2 input =
    let left, right =
        input
        |> Array.map parseLine
        |> Array.unzip
        |> Tuple.Map (fun (a, b) -> (Array.countBy id a), (Array.countBy id b |> dict))
    left
    |> Array.map (fun (num,count) ->
        if right.ContainsKey(num)
        then num * count * right[num]
        else 0)
    |> Array.sum

sample |> part1
input |> part1
sample |> part2
input |> part2