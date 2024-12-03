module AoC2024.Day1

open AoC2024.Prelude


let part1 fn () =
    let input =
        readInput fn
        |> Seq.map (splitS "   ")
        |> Array.transpose
        |> Array.map Array.sort

    let a = Array.head input |> Array.map int64
    let b = Array.last input |> Array.map int64
    let pairs = Array.zip a b
    pairs |> Array.map (fun t -> fst t - snd t |> abs) |> Array.sum

let part2 fn () =
    let input = readInput fn |> Seq.map (splitS "   ") |> Array.transpose
    let a = Array.head input |> Array.map int64
    let b = Array.last input |> Array.map int64
    let counts = b |> Array.countBy id |> Map.ofArray

    let getCount n =
        match counts |> Map.tryFind n with
        | Some c -> int64 c
        | None -> 0L

    a |> Array.map (fun n -> getCount n * n) |> Array.sum
