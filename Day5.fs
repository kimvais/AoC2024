module AoC2024.Day5

open AoC2024.Prelude

let parseRule s = s |> split '|' |> (fun [| a; b |] -> (int64 a, int64 b))

let getInput fn  =
    let input = readInputDelimByEmptyLine fn

    let rules =
        input
        |> Seq.head
        |> splitByLinefeed
        |> Seq.map parseRule
        |> Seq.groupBy fst
        |> Seq.map (fun (p, s) -> p, s |> Seq.map snd |> Set.ofSeq) |> Map.ofSeq

    let pages =
        input
        |> Seq.last
        |> splitByLinefeed
        |> Seq.map (split ',' >> Seq.map int64 >> List.ofSeq)
        |> List.ofSeq
    rules, pages

let rec checkOrdering rules (pageList: int64 list) =
    match pageList with
    | [] -> true
    | [_] -> true
    | head::tail ->
        let allowedPages = rules |> Map.tryFind head
        match allowedPages with
        | None -> false
        | Some a -> 
            let havePages = tail |> Set.ofList
            match havePages |> Set.isSuperset a with
            | true -> checkOrdering rules tail
            | false -> false
        
let part1 fn () =
    let rules, pages = getInput fn
    // printfn "%A" rules
    // printfn "%A" pages
    pages |> List.filter (checkOrdering rules) |> List.map (fun l -> l.[l.Length / 2]) |> List.sum

let part2 fn () = 
    0L