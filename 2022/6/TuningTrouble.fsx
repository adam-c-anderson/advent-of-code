// https://adventofcode.com/2022/day/6

open System
open System.IO

let input = File.ReadAllLines(Path.Join(__SOURCE_DIRECTORY__, "input.txt"))

let sample = (@"mjqjpqmgbljsphdztnvjfqwrcgsmlb".Split("\n", StringSplitOptions.TrimEntries))

module Datastream =
    let findStartMarker markerLength (buffer:string) =
        let startIndex =
            buffer.ToCharArray()
            |> Seq.windowed markerLength
            |> Seq.map (Array.distinct >> Array.length)
            |> Seq.findIndex ((=) markerLength)
        startIndex + markerLength

module Solution =
    let part1 (input:string[]) = input[0] |> Datastream.findStartMarker 4
    let part2 (input:string[]) = input[0] |> Datastream.findStartMarker 14

module Part1 =
    let sample = Solution.part1 sample // 7
    let answer = Solution.part1 input // 1361

module Part2 =
    let sample = Solution.part2 sample // 19
    let answer = Solution.part2 input // 3263