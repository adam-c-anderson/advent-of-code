// https://adventofcode.com/2022/day/5

open System
open System.IO
open System.Text.RegularExpressions

let input = File.ReadAllLines(Path.Join(__SOURCE_DIRECTORY__, "input.txt"))

let sample = (@"    [D]    
[N] [C]    
[Z] [M] [P]
 1   2   3 

move 1 from 2 to 1
move 3 from 1 to 3
move 2 from 2 to 1
move 1 from 1 to 2".Split("\n"))

type Move = { Count:int; From:int; To:int }

module Input =
    let split (ss:string[]) =
        let i = ss |> Array.findIndex String.IsNullOrWhiteSpace
        ss[..i-1],ss[i+1..]

module Stack =
    let push crate stack =
        if crate = ' '
        then stack
        else crate::stack

module Crate =
    let parse (ss:string[]) =
        let numStacks = 1 + (ss[0].Length - 3) / 4
        let stacks = Array.init numStacks (fun _ -> [])
        // This could be written as a foldBack, but I don't think that would make it any better
        for i in ss.Length-2..-1..0 do
            let s = ss[i]
            stacks[0] <- Stack.push s[1] stacks[0]
            for j in 1..numStacks-1 do
                stacks[j] <- Stack.push s[4 * j + 1] stacks[j]
        stacks
            
module Move =
    let parse =
        let re = Regex("""move (?<Count>\d+) from (?<From>\d+) to (?<To>\d+)""")
        Array.map (fun s ->
            let m = re.Match(s)
            {   Count = m.Groups["Count"].Value |> int
                From = m.Groups["From"].Value |> int
                To = m.Groups["To"].Value |> int
            })
    let exec (stacks:list<char>[]) move =
        let pushTo,newFrom = stacks[move.From-1] |> List.splitAt move.Count
        stacks[move.From-1] <- newFrom
        stacks[move.To-1] <- (List.rev pushTo) @ stacks[move.To-1]
        stacks
    let exec2 (stacks:list<char>[]) move =
        let pushTo,newFrom = stacks[move.From-1] |> List.splitAt move.Count
        stacks[move.From-1] <- newFrom
        stacks[move.To-1] <- pushTo @ stacks[move.To-1] // This is the only difference for Part 2-- no reversal of crates moved
        stacks

module Solution =
    let part1 input =
        let crateLines,moveLines = input |> Input.split
        let crates,moves = Crate.parse crateLines,Move.parse moveLines
        let finalStacks = (crates,moves) ||> Array.fold Move.exec
        String([| for stack in finalStacks -> stack[0] |])
    let part2 input =
        let crateLines,moveLines = input |> Input.split
        let crates,moves = Crate.parse crateLines,Move.parse moveLines
        let finalStacks = (crates,moves) ||> Array.fold Move.exec2 // <-- This is the only difference from Part 1
        String([| for stack in finalStacks -> stack[0] |])

module Part1 =
    let sample = Solution.part1 sample // "CMZ"
    let answer = Solution.part1 input // "CFFHVVHNC"

module Part2 =
    let sample = Solution.part2 sample // "MCD"
    let answer = Solution.part2 input // "FSZWBPTBG"