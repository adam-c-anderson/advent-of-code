// https://adventofcode.com/2021/day/2#part2

open System.Text.RegularExpressions
open System.IO

type Position = { Distance: int; Depth: int; Aim: int }

let (|Forward|Up|Down|) (s:string) =
    let m = Regex.Match(s, @"^(forward|up|down) (\d)$")
    match m.Groups[1].Value with
    | "forward" -> Forward (int m.Groups[2].Value)
    | "up" -> Up (int m.Groups[2].Value)
    | "down" -> Down (int m.Groups[2].Value)
    | _ -> failwith "Parse failure"

module Position =
    let move (pos:Position) = function
        | Forward x -> { pos with Distance = pos.Distance + x; Depth = pos.Depth + (x * pos.Aim) }
        | Up x -> { pos with Aim = pos.Aim - x}
        | Down x -> { pos with Aim = pos.Aim + x }

let sample = [|
    "forward 5"
    "down 5"
    "forward 8"
    "up 3"
    "down 8"
    "forward 2"
|]

let expected = sample |> Array.fold Position.move { Distance = 0; Depth = 0; Aim = 0 }

let input = File.ReadAllLines(Path.Join(__SOURCE_DIRECTORY__, "input.txt"))

let destination = input |> Array.fold Position.move { Distance = 0; Depth = 0; Aim = 0 }
destination.Distance * destination.Depth