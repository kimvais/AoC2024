module AoC2024.Day4

open AoC2024.Prelude

let xmas = "XMAS" |> Seq.map char |> List.ofSeq
let samx = xmas |> List.rev

let getAllDiagonals (matrix: char list list)= 
    let numRows = matrix.Length
    let numCols = if numRows > 0 then matrix.[0].Length else 0

    let rec collectDiagonal (row: int) (col: int) (dirRow: int) (dirCol: int) =
        let rec loop r c acc =
            if r >= 0 && r < numRows && c >= 0 && c < numCols then
                loop (r + dirRow) (c + dirCol) (matrix.[r].[c] :: acc)
            else List.rev acc
        loop row col []

    [
        for col in 0 .. numCols - 1 -> collectDiagonal 0 col 1 1
        for row in 1 .. numRows - 1 -> collectDiagonal row 0 1 1

        for col in 0 .. numCols - 1 -> collectDiagonal 0 col 1 -1
        for row in 1 .. numRows - 1 -> collectDiagonal row (numCols - 1) 1 -1
    ]

let findXmas = function
    | arr when arr = xmas || arr =  samx -> true
    | _ -> false

let getInput fn =
    readInput fn |> List.ofSeq |> List.map (Seq.map char >> List.ofSeq)

let part1 fn () =
    let input = getInput fn
    let lines = List.concat [input; List.transpose input; (getAllDiagonals input)]
    
    lines |> List.map (List.windowed 4 >> List.filter findXmas >> List.length) |> List.sum |> int64

let checkCorners (grid: char list list) x y  =
    let mas1 = [grid.[x-1].[y-1]; grid.[x+1].[y+1]] |> List.sort
    let mas2 = [grid.[x-1].[y+1]; grid.[x+1].[y-1]] |> List.sort
    match mas1,mas2 with
    | ['M'; 'S'], ['M'; 'S'] -> true
    | _ -> false
    
let findMas (grid: char list list) (x, y)=
    match grid.[x].[y] with
    | 'A' -> checkCorners grid x y
    | _ -> false
    
let part2 fn () =
    let input = getInput fn
    let maxx = input.Length - 2
    let maxy = input.[0].Length - 2
    let coords = Seq.allPairs [1..maxx] [1..maxy]
    coords |> Seq.filter (findMas input) |> Seq.length |> int64