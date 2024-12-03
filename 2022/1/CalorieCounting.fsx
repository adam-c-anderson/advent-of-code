// https://adventofcode.com/2022/day/1

open System
open System.IO

let input = File.ReadAllLines(Path.Join(__SOURCE_DIRECTORY__, "input.txt"))

let sample = (@"1000
2000
3000

4000

5000
6000

7000
8000
9000

10000".Split("\n", StringSplitOptions.TrimEntries))

module CalorieList =
    let parse (input: string[]) =
        let rec parseChunk (input: string[]) (output: ResizeArray<int[]>) =
            match input |> Array.tryFindIndex String.IsNullOrEmpty with
            | Some index ->
                output.Add(input |> Array.take index |> Array.map int)
                parseChunk (input |> Array.skip (index + 1)) output
            | None ->
                output.Add(input |> Array.map int)
                output.ToArray()
        parseChunk input (ResizeArray())

module Solution =
    let part1 =
        input
        |> CalorieList.parse
        |> Array.map Array.sum
        |> Array.max
    let part2 =
        input
        |> CalorieList.parse
        |> Array.map Array.sum
        |> Array.sortDescending
        |> Array.take 3
        |> Array.sum