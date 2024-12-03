open System.IO

let sample = [| "3,4,3,1,2" |]

let input = File.ReadAllLines(Path.Join(__SOURCE_DIRECTORY__, "input.txt"))

module Lanternfish =
    let parse (input:string[]) =
        input
        |> Array.collect (fun s -> s.Split(',') |> Array.map int)
        |> Array.countBy id
        |> Array.map (fun (timer,count) -> (timer,int64 count))

    let tick (timer:int,count:int64) =
        match timer with
        | 0 -> [| (6,count); (8,count) |]
        | n -> [| (n-1,count) |]
    
    let rec simulate (days:int) (fishState:(int*int64)[]) =
        match days with
        | 0 -> fishState
        | n ->
            fishState
            |> Array.collect tick
            |> Array.groupBy fst
            |> Array.map (fun (key,items) -> (key, items |> Array.sumBy snd))
            |> simulate (n - 1)

input
|> Lanternfish.parse
|> Lanternfish.simulate 256
|> Array.sumBy snd