// https://adventofcode.com/2024/day/3

open System.IO
open System.Text.RegularExpressions

let sample = File.ReadAllLines(Path.Join(__SOURCE_DIRECTORY__, "sample.txt"))
let input = File.ReadAllLines(Path.Join(__SOURCE_DIRECTORY__, "input.txt"))

let parseLine line = [
    for m in Regex.Matches(line, @"mul\((\d{1,3},\d{1,3})\)") ->
        let x,y = m.Groups[1].Value.Split(',') |> Array.pairwise |> Array.head
        (int x),(int y)
]

type Instruction =
    | Do
    | Dont
    | Mul of int * int

let parseLine2 line = [
    for m in Regex.Matches(line, @"do\(\)|don't\(\)|mul\((\d{1,3},\d{1,3})\)") ->
        match m.Value with
        | "do()" -> Do
        | "don't()" -> Dont
        | _ -> m.Groups[1].Value.Split(',') |> Array.map int |> Array.pairwise |> Array.head |> Mul
]

let part1 input =
    input
    |> Array.map parseLine
    |> List.concat
    |> List.map (fun (x, y) -> x * y)
    |> List.sum

let part2 input =
    let instructions = 
        input
        |> Array.map parseLine2
        |> List.concat
    let doInstructions =
        let mutable enabled = true
        [
            for i in instructions do
                match i with
                | Do -> enabled <- true
                | Dont -> enabled <- false
                | Mul (x,y) -> if enabled then yield x * y
        ]
    List.sum doInstructions

[|"xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))"|] |> part1
input |> part1
[|"xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))"|] |> part2
input |> part2