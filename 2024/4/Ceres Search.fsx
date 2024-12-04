// https://adventofcode.com/2024/day/4

open System.IO

let sample = File.ReadAllLines(Path.Join(__SOURCE_DIRECTORY__, "sample.txt"))
let input = File.ReadAllLines(Path.Join(__SOURCE_DIRECTORY__, "input.txt"))

let rec findNextIndex (input:string[]) (line, col) (ofChar:char) =
    if line >= input.Length
    then None
    elif (col + 1) = input[line].Length
    then findNextIndex input (line + 1, -1) ofChar
    else
        match input[line].IndexOf(ofChar, col + 1) with
        | -1 -> findNextIndex input (line + 1, -1) ofChar
        | idx -> Some (line, idx)

let findXmasFrom (input:string[]) (line, col) =
    let vectors = [
        for dx in -1..1 do
            for dy in -1..1 do
                if (dx, dy) <> (0, 0)
                then [ for i in 1..3 -> (dx * i, dy * i) ]
    ]
    let words = [
        for v in vectors do [|
            for x, y in v do
                let liney = line + y
                let colx = col + x
                if liney < 0 || liney >= input.Length || colx < 0 || colx >= input[line].Length 
                then ' '
                else input[liney][colx]
        |]
    ]
    words |> List.map System.String |> List.filter (fun word -> word = "MAS") |> List.length

let rec findAllXmas (input:string[]) position count =
    match findNextIndex input position 'X' with
    | None -> count
    | Some xPos ->
        let xmasCount = findXmasFrom input xPos
        findAllXmas input xPos (count + xmasCount)

let part1 (input:string[]) =
    findAllXmas input (0, -1) 0
 
let findMasXFrom (input:string[]) (line, col) =
    let vectors = [
        for dx in [-1; 1] do
            for dy in [-1; 1] do
                [ (dx, dy); (0, 0); (-dx, -dy) ]
    ]
    let words = [
        for v in vectors do [|
            for x, y in v do
                let liney = line + y
                let colx = col + x
                if liney < 0 || liney >= input.Length || colx < 0 || colx >= input[line].Length 
                then ' '
                else input[liney][colx]
        |]
    ]
    if words |> List.map System.String |> List.forall (fun word -> word = "MAS" || word = "SAM")
    then 1
    else 0

let rec findAllMasX (input:string[]) position count =
    match findNextIndex input position 'A' with
    | None -> count
    | Some xPos ->
        let xmasCount = findMasXFrom input xPos
        findAllMasX input xPos (count + xmasCount)

let part2 input =
    findAllMasX input (0, -1) 0

sample |> part1
input |> part1
sample |> part2
input |> part2