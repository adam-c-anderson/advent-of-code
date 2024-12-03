open System.IO

let sample = [| "3,4,3,1,2" |]

let input = File.ReadAllLines(Path.Join(__SOURCE_DIRECTORY__, "input.txt"))

module Lanternfish =
    let parse (input:string[]) =
        input
        |> Array.collect (fun s -> s.Split(',') |> Array.map int)

    let tick (timer:int) =
        match timer with
        | 0 -> [| 6; 8 |]
        | n -> [| n - 1 |]
    
    let rec simulate (days:int) (fishState:int[]) =
        match days with
        | 0 -> fishState
        | n ->
            fishState
            |> Array.collect tick
            |> simulate (n - 1)

input
|> Lanternfish.parse
|> Lanternfish.simulate 80
|> Array.length