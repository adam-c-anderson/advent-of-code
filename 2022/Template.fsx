// https://adventofcode.com/2022/day/N

open System
open System.IO

let input = File.ReadAllLines(Path.Join(__SOURCE_DIRECTORY__, "input.txt"))

let sample = (@"".Split("\n", StringSplitOptions.TrimEntries))

module Solution =
    let part1 input = ()
    let part2 input = ()

module Part1 =
    let sample = Solution.part1 sample // 2
    let answer = Solution.part1 input // 453

module Part2 =
    let sample = Solution.part2 sample // 4
    let answer = Solution.part2 input // 919