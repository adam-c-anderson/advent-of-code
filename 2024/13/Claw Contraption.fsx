// https://adventofcode.com/2024/day/13
#r "nuget: MathNet.Numerics"
#r "nuget: MathNet.Numerics.FSharp"

open System.IO
open System.Text.RegularExpressions
open MathNet.Numerics.LinearAlgebra

let sample = File.ReadAllLines(Path.Join(__SOURCE_DIRECTORY__, "sample.txt"))
let input = File.ReadAllLines(Path.Join(__SOURCE_DIRECTORY__, "input.txt"))

type Button = {
    X: float
    Y: float
    Cost: float
}

type Problem = {
    ButtonA: Button
    ButtonB: Button
    Prize: float * float
}

let parse (input:string[]) =
    let parseButton (line:string) =
        let m = Regex.Match(line, @"X\+(\d+), Y\+(\d+)")
        float(m.Groups[1].Value),float(m.Groups[2].Value)

    let parsePrize (line:string) =
        let m = Regex.Match(line, @"X=(\d+), Y=(\d+)")
        float(m.Groups[1].Value),float(m.Groups[2].Value)
        
    [
        for i in 0..4..input.Length - 3 ->
            {
                ButtonA=
                    let x,y = parseButton input[i]
                    { X=x; Y=y; Cost=3.0 }
                ButtonB=
                    let x,y = parseButton input[i+1]
                    { X=x; Y=y; Cost=1.0 }
                Prize=parsePrize input[i+2]
            }
    ]

// Does not scale well for part 2 :)
let listPrizeSolutions { ButtonA=btnA; ButtonB=btnB; Prize=(prizeX,prizeY) } = seq {
    let maxA = 1.0 + min (prizeX / btnA.X) (prizeY / btnA.Y)
    for i in 0.0..maxA do
        let bx,by = (prizeX - btnA.X * i),(prizeY - btnA.Y * i)
        if round(bx % btnB.X) = 0.0 && round(by % btnB.Y) = 0.0 && round(bx / btnB.X) = round(by / btnB.Y)
        then Some (round(i * btnA.Cost) + round((bx / btnB.X) * btnB.Cost))
        else None
}

let solveEquation { ButtonA=btnA; ButtonB=btnB; Prize=(prizeX,prizeY) } =
    let m = matrix [    [ btnA.X; btnB.X ]
                        [ btnA.Y; btnB.Y ] ]
    let v = vector [ prizeX; prizeY ]
    let solution = m.Solve(v)
    let a,b = solution[0],solution[1]
    if (round(a) * btnA.X + round(b) * btnB.X = prizeX)
    then round(btnA.Cost * a) + round(btnB.Cost * b)
    else 0.0

let part1 input =
    input
    |> parse
    |> List.map (listPrizeSolutions >> (Seq.choose id))
    |> List.filter (not<<Seq.isEmpty)
    |> List.map Seq.min
    |> List.sum

let part2 input =
    input
    |> parse
    |> List.map (fun ({ Prize=(x,y) } as p) -> { p with Prize=(x + 10000000000000.0, y + 10000000000000.0)})
    |> List.map solveEquation
    |> List.sum

input |> part2 |> sprintf "%.0f"