module AoC2024.Tests
open AoC2024


open FsUnit.Xunit
open Xunit


[<Fact>]
let ``day 1, part 1`` () =
    Day1.part1 "test1" () |> should equal 11L
    Day1.part1 "1" () |> should equal 2031679L

[<Fact>]
let ``day1, part2`` () =
    Day1.part2 "test1" () |> should equal 31L
    Day1.part2 "1" () |> should equal 19678534L

[<Fact>]
let ``day2, part1`` () =
    Day2.part1 "test2" () |> should equal 2L
    Day2.part1 "2" () |> should equal 585L

[<Fact>]
let ``day2, part2`` () =
    Day2.part2 "test2" () |> should equal 4L
    Day2.part2 "2" () |> should equal 626L
