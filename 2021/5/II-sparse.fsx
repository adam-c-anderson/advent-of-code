open System
open System.IO
open System.Text.RegularExpressions

let sample = [|
    "0,9 -> 5,9"
    "8,0 -> 0,8"
    "9,4 -> 3,4"
    "2,2 -> 2,1"
    "7,0 -> 7,4"
    "6,4 -> 2,0"
    "0,9 -> 2,9"
    "3,4 -> 1,4"
    "0,0 -> 8,8"
    "5,5 -> 8,2"
|]

let input = File.ReadAllLines(Path.Join(__SOURCE_DIRECTORY__, "input.txt"))

type Point = int * int
type Line = Point * Point

module Line =
    let parse (s:string): Line =
        let m = Regex.Match(s, @"(\d+),(\d+) -> (\d+),(\d+)")
        (int m.Groups[1].Value,int m.Groups[2].Value),(int m.Groups[3].Value,int m.Groups[4].Value)

    let toPoints (((x1,y1),(x2,y2)):Line) =
        if (x1 = x2) then
            let dy = if y1 < y2 then 1 else -1
            [ for y in y1..dy..y2 -> x1,y ]
        elif (y1 = y2) then
            let dx = if x1 < x2 then 1 else -1
            [ for x in x1..dx..x2 -> x,y1 ]
        else // Assume 45 degrees
            let dx = if x1 < x2 then 1 else -1
            let dy = if y1 < y2 then 1 else -1
            List.zip [x1..dx..x2] [y1..dy..y2]

    let countIntersections (lines:Line list) =
        lines
        |> List.collect toPoints
        |> List.countBy id
        |> List.filter (fun (_,count) -> count > 1)
        |> List.length

let lines = input |> Array.map Line.parse

lines
|> List.ofArray
|> Line.countIntersections