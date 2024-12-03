// https://adventofcode.com/2022/day/4

open System
open System.IO

let input = File.ReadAllLines(Path.Join(__SOURCE_DIRECTORY__, "input.txt"))

let sample = (@"2-4,6-8
2-3,4-5
5-7,7-9
2-8,3-7
6-6,4-6
2-6,4-8".Split("\n", StringSplitOptions.TrimEntries))

module Pair =
    let mapBoth fn (x,y) = fn x,fn y

module String =
    let splitPair (separator:char) (input:string) =
        let items = input.Split(separator)
        items[0],items[1]

module List =
    let ofRangePair (x,y) = [x..y]

module SectionAssignments =
    let parse (rawAssignments:string[]) =
        [ for rawAssignment in rawAssignments ->
            rawAssignment
            |> String.splitPair ','
            |> Pair.mapBoth (String.splitPair '-' >> Pair.mapBoth int >> List.ofRangePair >> Set.ofList) ]

module Solution =
    let part1 input =
        input
        |> SectionAssignments.parse
        |> List.filter (fun (set1,set2) -> set1.IsSubsetOf(set2) || set2.IsSubsetOf(set1))
        |> List.length
        
    let part2 input =
        input
        |> SectionAssignments.parse
        |> List.filter (fun (set1,set2) -> Set.intersect set1 set2 |> Set.isEmpty |> not)
        |> List.length

module Part1 =
    let sample = Solution.part1 sample // 2
    let answer = Solution.part1 input // 453

module Part2 =
    let sample = Solution.part2 sample // 4
    let answer = Solution.part2 input // 919