open System
open System.IO
open System.Text

let sample = [|
    "00100"
    "11110"
    "10110"
    "10111"
    "10101"
    "01111"
    "00111"
    "11100"
    "10000"
    "11001"
    "00010"
    "01010"
|]

let input = File.ReadAllLines(Path.Join(__SOURCE_DIRECTORY__, "input.txt"))

module Array =
    let pivot (xss:'a[][]) = [| for i in 0..(xss[0].Length - 1) -> xss |> Array.map (fun xs -> xs[i]) |]

module DiagnosticReport =
    let analyze (report:string[]) =
        let charCounts =    report
                            |> Array.map (fun x -> x.ToCharArray())
                            |> Array.pivot
                            |> Array.map (Array.countBy id)

        ((StringBuilder(), StringBuilder()), charCounts)
        ||> Array.fold (fun (gamma,epsilon) counts -> 
            match counts with
            | [|('0',_)|] -> (gamma.Append('0'),epsilon.Append('1'))
            | [|('1',_)|] -> (gamma.Append('1'),epsilon.Append('0'))
            | [|(char0,count0);(char1,count1)|] ->
                if (count0 = count1) then
                    (gamma.Append('1'),epsilon.Append('0'))
                elif count0 > count1 then
                    (gamma.Append(char0),epsilon.Append(char1))
                else
                    (gamma.Append(char1),epsilon.Append(char0))
            | x -> failwithf "Unexpected input format: %A" x)
        |> fun (gamma,epsilon) -> (gamma.ToString(),epsilon.ToString())

    let findRating selectCriteria (report:string[]) =
        let rec search i (remaining:string[]) =
            match remaining with
            | [|single|] -> single
            | multiple ->
                let criteria:string = remaining |> analyze |> selectCriteria
                multiple |> Array.filter (fun s -> s[i] = criteria[i]) |> search (i + 1)
        search 0 report

let main report =
    let gammaRating = DiagnosticReport.findRating fst report
    let epsilonRating = DiagnosticReport.findRating snd report
    Convert.ToInt32(gammaRating, 2) * Convert.ToInt32(epsilonRating, 2)

main input