module AoC2024.Day3

open System.Text.RegularExpressions
open AoC2024.Prelude

type Command =
    | Enable
    | Disable
    | Multiply

let parseCmd =
    function
    | "do" -> Enable
    | "don't" -> Disable
    | "mul" -> Multiply
    | c -> failwith ($"Invalid command: {c}")

type State = { Acc: int64; Enabled: bool }

let executeCmd state m =
    match parseCmd (m: Match).Groups["cmd"].Value with
    | Enable -> { state with Enabled = true }
    | Disable -> { state with Enabled = false }
    | Multiply ->
        let res =
            (int64 (m: Match).Groups["a"].Value)
            * (int64 (m: Match).Groups["b"].Value)

        match state.Enabled with
        | true -> { state with Acc = state.Acc + res }
        | false -> state

let part1 fn () =
    let input = readAsText fn
    let re = Regex(@"mul\((?<a>\d{1,3}),(?<p>\d{1,3})\)")

    re.Matches(input)
    |> Seq.map (
        _.Groups
        >> Seq.tail
        >> (Seq.map (_.Value >> int64))
        >> Seq.reduce (*)
    )
    |> Seq.sum

let part2 fn () =
    let input = readAsText fn

    let re2 =
        Regex(@"(?<cmd>do|don't|mul)\(((?<a>\d{1,3}),(?<b>\d{1,3}))?\)", RegexOptions.ExplicitCapture)

    let state = { Acc = 0L; Enabled = true }

    re2.Matches(input)
    |> Seq.fold executeCmd state
    |> _.Acc
