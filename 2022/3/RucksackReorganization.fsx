// https://adventofcode.com/2022/day/3

open System
open System.IO

let input = File.ReadAllLines(Path.Join(__SOURCE_DIRECTORY__, "input.txt"))

let sample = (@"vJrwpWtwJgWrhcsFMMfFFhFp
jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
PmmdzqPrVvPwwTWBwg
wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
ttgJtRGJQctTZtZT
CrZsJsPPZsGzwwsLwLmpwMDw".Split("\n", StringSplitOptions.TrimEntries))

module Item =
    let value =
        let values = ['a'..'z']@['A'..'Z'] |> List.mapi (fun i c -> (c, i + 1)) |> Map.ofList
        fun item -> values[item]

module Rucksack =
    let parse (s:string) = s.ToCharArray()
    let compartmentalize xs = xs |> Array.chunkBySize (xs.Length / 2) |> Array.map Set.ofArray
    let findDuplicate sets = sets |> Array.reduce Set.intersect |> Set.minElement

module Solution =
    let part1 input = input |> Array.map (Rucksack.parse >> Rucksack.compartmentalize >> Rucksack.findDuplicate >> Item.value) |> Array.sum
    let part2 input = input |> Array.map (Rucksack.parse >> Set.ofArray) |> Array.chunkBySize 3 |> Array.map (Rucksack.findDuplicate >> Item.value) |> Array.sum

module Part1 =
    let sample = Solution.part1 sample // 157
    let answer = Solution.part1 input // 8039

module Part2 =
    let sample = Solution.part2 sample // 70
    let answer = Solution.part2 input // 2510