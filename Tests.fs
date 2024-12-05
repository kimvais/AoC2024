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

[<Fact>]
let ``day3, part1`` () =
    Day3.part1 "test3" () |> should equal 161L
    Day3.part1 "3" () |> should equal 160672468L

[<Fact>]
let ``day3, part2`` () =
    Day3.part2 "test3b" () |> should equal 48L
    Day3.part2 "3" () |> should equal 84893551L
    
[<Fact>]
let ``day4, part1`` () =
    Day4.part1 "test4" () |> should equal 18L
    Day4.part1 "4" () |> should equal 2401L
    
[<Fact>]
let ``day4, part2`` ()=
    Day4.part2 "test4" () |> should equal 9L
    Day4.part2 "4" () |> should equal 1822L
    
[<Fact>]
let ``day5, part1`` ()=
    Day5.part1 "test5" () |> should equal 143L
    Day5.part1 "5" () |> should equal 5452L 
    
[<Fact>]
let ``day5, part2`` ()=
    Day5.part2 "test5" () |> should equal 0L
    Day5.part2 "5" () |> should equal 0L 
    
