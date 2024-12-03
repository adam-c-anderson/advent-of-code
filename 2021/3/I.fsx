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
            | [|(char0,count0);(char1,count1)|] ->
                if count0 > count1 then
                    (gamma.Append(char0),epsilon.Append(char1))
                else
                    (gamma.Append(char1),epsilon.Append(char0))
            | _ -> failwith "Unexpected input format")
        |> fun (gamma,epsilon) -> (gamma.ToString(),epsilon.ToString())

let main report =
    let (gamma,epsilon) = DiagnosticReport.analyze report
    Convert.ToInt32(gamma, 2) * Convert.ToInt32(epsilon, 2)

main input