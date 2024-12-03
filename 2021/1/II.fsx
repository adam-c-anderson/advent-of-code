// https://adventofcode.com/2021/day/1#part2

 open System.IO

 let input = File.ReadAllLines(Path.Join(__SOURCE_DIRECTORY__, "input.txt"))

 input
 |> Seq.map int
 |> Seq.windowed 3 // The simpler way :)
 |> Seq.map Array.sum
 |> Seq.pairwise
 |> Seq.filter (fun (x,y) -> x < y)
 |> Seq.length