// https://adventofcode.com/2022/day/2

open System
open System.IO

let input = File.ReadAllLines(Path.Join(__SOURCE_DIRECTORY__, "input.txt"))

let sample = (@"A Y
B X
C Z".Split("\n", StringSplitOptions.TrimEntries))

type Shape = | Rock | Paper | Scissors

type Outcome = | Lose | Draw | Win

module Shape =
    let parseOpponent = function | 'A' -> Rock | 'B' -> Paper | 'C' -> Scissors | _ -> failwith "Invalid opponent move"
    let parseResponse = function | 'X' -> Rock | 'Y' -> Paper | 'Z' -> Scissors | _ -> failwith "Invalid response move"
    let parse (s:string) = (parseOpponent s[0], parseResponse s[2])
    let score = function | Rock -> 1 | Paper -> 2 | Scissors -> 3

module Outcome =
    let score = function | Lose -> 0 | Draw -> 3 | Win -> 6
    let parse = function | 'X' -> Lose | 'Y' -> Draw | 'Z' -> Win | _ -> failwith "Invalid outcome"

module Strategy =
    let parse (s:string) = (Shape.parseOpponent s[0], Outcome.parse s[2])
    let toShape = function
        | (Rock,Draw) | (Paper,Lose) | (Scissors,Win) -> Rock
        | (Rock,Win) | (Paper,Draw) | (Scissors,Lose) -> Paper
        | (Rock,Lose) | (Paper,Win) | (Scissors,Draw) -> Scissors
    let toMoves strategy = (fst strategy,toShape strategy)

module Game =
    let play = function
        | (Rock,Scissors) | (Paper,Rock) | (Scissors,Paper) -> Lose
        | (Rock,Rock) | (Paper,Paper) | (Scissors,Scissors) -> Draw
        | (Rock,Paper) | (Paper,Scissors) | (Scissors,Rock) -> Win
    let score moves = (moves |> play |> Outcome.score) + (moves |> snd |> Shape.score)

module Solution =
    let part1 input = input |> Array.map (Shape.parse >> Game.score) |> Array.sum
    let part2 input = input |> Array.map (Strategy.parse >> Strategy.toMoves >> Game.score) |> Array.sum

module Part1 =
    let sample = Solution.part1 sample
    let answer = Solution.part1 input

module Part2 =
    let sample = Solution.part2 sample
    let answer = Solution.part2 input