module AoC2024.Day2

open AoC2024.Prelude

type Pair =
    | Inc
    | Dec
    | Invalid

let compare t =
    match fst t - snd t with
    | -1L
    | -2L
    | -3L -> Dec
    | 1L
    | 2L
    | 3L -> Inc
    | _ -> Invalid

let test arr =
    Array.contains Invalid arr |> not && Array.length arr = 1

let part1 fn () =
    let input =
        readInput fn
        |> Seq.map (split ' ')
        |> Array.ofSeq
        |> Array.map (Array.map int64 >> Array.pairwise)

    input
    |> Array.map (fun arr -> arr |> Array.map compare |> Array.groupBy id |> Array.map fst)
    |> Array.filter test
    |> Array.length
    |> int64


let part2 fn () = 0L
