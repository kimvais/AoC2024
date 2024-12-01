module AoC2024.Day1

open AoC2024.Prelude


let part1 fn () =
    let input = readInput fn |> Seq.map (splitS "   ") |> Seq.transpose |> Seq.map Seq.sort
    let a = Seq.head input |> Seq.map int64
    let b = Seq.last input |> Seq.map int64
    let pairs =  Seq.zip a b 
    pairs |> Seq.map (fun t -> fst t - snd t |> abs) |> Seq.sum
   
let part2 fn () = 
    let input = readInput fn |> Seq.map (splitS "   ") |> Seq.transpose 
    let a = Seq.head input |> Seq.map int64
    let b = Seq.last input |> Seq.map int64
    let counts = b |> Seq.countBy id |> Map.ofSeq
    let getCount n =
        match counts |> Map.tryFind n with
        | Some c -> int64 c
        | None -> 0L
    a |> Seq.map (fun n -> getCount n * n) |> Seq.sum

