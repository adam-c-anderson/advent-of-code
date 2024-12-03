// https://adventofcode.com/2022/day/7

open System
open System.IO

let input = File.ReadAllLines(Path.Join(__SOURCE_DIRECTORY__, "input.txt"))

let sample = (@"$ cd /
$ ls
dir a
14848514 b.txt
8504156 c.dat
dir d
$ cd a
$ ls
dir e
29116 f
2557 g
62596 h.lst
$ cd e
$ ls
584 i
$ cd ..
$ cd ..
$ cd d
$ ls
4060174 j
8033020 d.log
5626152 d.ext
7214296 k".Split("\n", StringSplitOptions.TrimEntries))

type TerminalCommand =
    | Cd of string
    | Ls

type File = { Name: string; Size: int }

type TerminalOutput =
    | Dir of string list * string
    | File of string list * File

type TerminalLine =
    | Command of TerminalCommand
    | Output of TerminalOutput

module TerminalCommand =
    let parse (s:string) =
        match s[0..3] with
        | "$ ls" -> Ls
        | "$ cd" -> Cd s[5..]
        | _ -> failwithf "Unrecognized command %s" s

module TerminalOutput =
    let parse (location:string list,line:string) =
        match line[0..2] with
        | "dir" -> Dir (location, line[4..])
        | _ ->
            let ss = line.Split(' ')
            File (location, { Size = int(ss[0]); Name = ss[1] })

module TerminalLine =
    let parse (location:string list,line:string) =
        match line[0] with
        | '$' -> TerminalCommand.parse line |> Command
        | _ -> TerminalOutput.parse (location,line) |> Output

module Path =
    let up (location:string list) = location |> List.removeAt (location.Length - 1)
    let down (location:string list) (dir:string) = location |> List.insertAt (location.Length) dir

module Location =
    let segsToPaths (segs:string list) = ("/",segs) ||> List.scan (fun path seg -> $"{path}{seg}/")

module Filesystem =
    let parseLine (location:string list,files:(string list * File) list) (line:string) =
        match TerminalLine.parse (location,line) with
        | Command (Cd "/") -> ([],files)
        | Command (Cd "..") -> (Path.up location,files)
        | Command (Cd dir) -> (Path.down location dir,files)
        | Output (File (location,file)) -> (location,(location,file)::files)
        | _ -> (location,files)
    let parseLines (lines:string[]) =
        (([],[]),lines)
        ||> Array.fold parseLine
        |> snd
    let measureDirs pathSegFiles =
        pathSegFiles
        |> List.collect (fun (segs,file) -> Location.segsToPaths segs |> List.map (fun path -> (path,file)))
        |> List.groupBy fst
        |> List.map (fun (path,pathFiles) -> (path,pathFiles |> List.sumBy (fun (_,file) -> file.Size)))

module Solution =
    let part1 input =
        input
        |> Filesystem.parseLines
        |> Filesystem.measureDirs
        |> List.filter (fun (_,size) -> size <= 100_000)
        |> List.sumBy snd

    let part2 input = 
        let dirSizes =  input
                        |> Filesystem.parseLines
                        |> Filesystem.measureDirs
        let rootSize =  dirSizes
                        |> List.find (fun (path,size) -> path = "/")
                        |> snd
        let diskCapacity = 70_000_000
        let spaceNeeded = 30_000_000
        let spaceAvailable = diskCapacity - rootSize
        let minSizeToDelete = spaceNeeded - spaceAvailable
        dirSizes
        |> List.filter (fun (_,size) -> size >= minSizeToDelete)
        |> List.minBy snd

module Part1 =
    let sample = Solution.part1 sample // 95437
    let answer = Solution.part1 input // 1490523

module Part2 =
    let sample = Solution.part2 sample // ("/d/", 24933642)
    let answer = Solution.part2 input // ("/qcznqph/pdtpt/qtbprrq/sbtl/", 12390492)