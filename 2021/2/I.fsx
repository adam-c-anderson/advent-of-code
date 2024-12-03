// https://adventofcode.com/2021/day/2

open System.IO

type Position = { Distance: int; Depth: int }

let (|Forward|Up|Down|) (s:string) =
    match s.Split(' ') with
    | [|"forward"; x|] -> Forward (int x)
    | [|"up"; x|] -> Up (int x)
    | [|"down"; x|] -> Down (int x)
    | _ -> failwith "Parse failure"

module Position =
    let move (pos:Position) = function
        | Forward x -> { pos with Distance = pos.Distance + x }
        | Up x -> { pos with Depth = pos.Depth - x }
        | Down x -> { pos with Depth = pos.Depth + x }

let sample = [|
    "forward 5"
    "down 5"
    "forward 8"
    "up 3"
    "down 8"
    "forward 2"
|]

let expected = sample |> Array.fold Position.move { Distance = 0; Depth = 0 }

let input = File.ReadAllLines(Path.Join(__SOURCE_DIRECTORY__, "input.txt"))

let destination = input |> Array.fold Position.move { Distance = 0; Depth = 0 }
destination.Distance * destination.Depth