open System
open System.IO

let sample = [|
    "7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1"
    ""
    "22 13 17 11  0"
    " 8  2 23  4 24"
    "21  9 14 16  7"
    " 6 10  3 18  5"
    " 1 12 20 15 19"
    ""
    " 3 15  0  2 22"
    " 9 18 13 17  5"
    "19  8  7 25 23"
    "20 11 10 24  4"
    "14 21 16 12  6"
    ""
    "14 21 17 24  4"
    "10 16 15  9 19"
    "18  8 23 26 20"
    "22 11 13  6  5"
    " 2  0 12  3  7"
|]

let input = File.ReadAllLines(Path.Join(__SOURCE_DIRECTORY__, "input.txt"))

let BOARD_SIZE = 5

type BingoNumber = { Number: int; Position: int * int }

type BingoBoard = { Numbers: BingoNumber list; MarkedPositions: (int * int) list }

module BingoBoard =
    let parse (boardStrings: string list) =
        let rec parseRow (row: int) (rowString: string) =
            rowString.Split(' ', StringSplitOptions.RemoveEmptyEntries)
            |> Seq.mapi (fun i s -> { Number = int s; Position = (row,i)})
            |> List.ofSeq
        let parseBoard rowStrings = { Numbers = rowStrings |> List.mapi parseRow |> List.concat; MarkedPositions = [] }
        let rec parseBoards (boardStrings: string list) (boards: BingoBoard list) =
            match boardStrings with
            | [] -> boards
            | ""::tail -> parseBoards tail boards
            | rowStrings ->
                let board = rowStrings |> List.take BOARD_SIZE |> parseBoard
                parseBoards (List.skip BOARD_SIZE rowStrings) (board::boards)
        parseBoards boardStrings []
    
    let mark (number: int) (board: BingoBoard) =
        board.Numbers
        |> List.tryPick (fun bingoNumber -> if bingoNumber.Number = number then Some bingoNumber.Position else None)
        |> function
            | Some position -> { board with MarkedPositions = position::board.MarkedPositions }
            | None -> board

    let wins { MarkedPositions = positions } =
        (positions |> List.countBy fst |> List.exists (fun (_,cnt) -> cnt = BOARD_SIZE)) ||
        (positions |> List.countBy snd |> List.exists (fun (_,cnt) -> cnt = BOARD_SIZE))

    let score (winningNumber: int) (board: BingoBoard) =
        let unmarkedNumberSum =
            board.Numbers
            |> List.choose (fun { Number = num; Position = pos } -> if List.contains pos board.MarkedPositions
                                                                    then None
                                                                    else Some num)
            |> List.sum
        unmarkedNumberSum * winningNumber


module BingoGame =
    let parse (gameStrings: string[]) =
        let markedNumbers = gameStrings[0].Split(',') |> Seq.map int |> List.ofSeq
        let boards = gameStrings[2..] |> List.ofArray |> BingoBoard.parse
        (boards, markedNumbers)

    let rec play (boards: BingoBoard list) (markedNumbers: int list) =
        match markedNumbers with
        | [] -> None
        | headNumber::tailNumbers ->
            // Slight inefficiency here; marking all boards before checking for a win
            let markedBoards = boards |> List.map (BingoBoard.mark headNumber)
            match markedBoards |> List.tryFind BingoBoard.wins with // Assuming only one winning board per number drawn
            | Some winner -> Some (BingoBoard.score headNumber winner)
            | None -> play markedBoards tailNumbers

    let findLastWinner (boards: BingoBoard list) (markedNumbers: int list) =
        let rec flw (boards: BingoBoard list) (markedNumbers: int list) (winners: (int * BingoBoard) list) =
            match boards,markedNumbers with
            | [],_
            | _,[] -> List.tryHead winners |> Option.map (fun (winningNumber,board) -> BingoBoard.score winningNumber board)
            | _,headNumber::tailNumbers ->
                let newWinners,nextBoards = boards |> List.map (BingoBoard.mark headNumber) |> List.partition BingoBoard.wins
                let newWinnersWithNumber = newWinners |> List.map (fun nw -> headNumber,nw)
                flw nextBoards tailNumbers (newWinnersWithNumber @ winners) // Probably only need to keep the latest one, now that I think about it
        flw boards markedNumbers []

input |> BingoGame.parse ||> BingoGame.findLastWinner