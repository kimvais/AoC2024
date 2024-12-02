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

let test arr = Array.contains Invalid arr |> not && Array.length arr = 1

let solve input =
    input
    |> Array.map (fun arr -> arr |> Array.map compare |> Array.groupBy id |> Array.map fst)
    |> Array.filter test

let getInput fn =
    readInput fn
    |> Seq.map (split ' ')
    |> Array.ofSeq
    |> Array.map (Array.map int64)

let removeEachElement arr =
    arr
    |> Array.mapi (fun i _ ->
        arr
        |> Array.mapi (fun j x -> if i <> j then Some x else None)
        |> Array.choose id)

let part1 fn () = getInput fn |> Array.map Array.pairwise |> solve |> Array.length |> int64

let part2 fn () =
    let input = getInput fn

    input
    |> Array.map (
        removeEachElement
        >> (Array.map Array.pairwise)
    )
    |> Array.map solve
    |> Array.filter (Array.isEmpty >> not)
    |> Array.length
    |> int64

