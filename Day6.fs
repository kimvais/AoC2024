module AoC2024.Day6

open AoC2024.Prelude

type Direction =
    | North
    | East
    | South
    | West

type Guard = { Facing: Direction; X: int; Y: int; Visited: Guard list; Loops: (int*int) list }

type Position =
    | Guard of Guard
    | Obstacle of int * int


let extractPositions (positions: seq<Position>) =
    let guard =
        positions
        |> Seq.choose (function
            | Guard g -> Some g
            | _ -> None)
        |> Seq.exactlyOne

    let obstacles =
        positions
        |> Seq.choose (function
            | Obstacle(x, y) -> Some(x, y)
            | _ -> None)
        |> Set.ofSeq

    guard, obstacles

let turn g =
    match g.Facing with
    | North -> { g with Facing = East }
    | East -> { g with Facing = South }
    | South -> { g with Facing = West }
    | West -> { g with Facing = North }

let solve fn =
    let input = readInput fn

    let maxy = input |> Seq.length
    let maxx = input |> Seq.head |> Seq.length

    let state =
        input
        |> Seq.mapi (fun y row ->
            row
            |> Seq.mapi (fun x pos ->
                match pos with
                | '.' -> None
                | '#' -> Some(Obstacle(x, y))
                | '^' ->
                    let g = { X = x; Y = y; Visited = List.empty; Facing = North; Loops=[]}
                    Some(Guard { X = x; Y = y; Visited = [ g ]; Facing = North; Loops =[] })))
        |> Seq.concat
        |> Seq.choose id

    let guard, obstacles = extractPositions state
    
    let checkForLoop g =
       g.Visited |> List.exists (fun g' -> g'.X = g.X && g'.Y = g.Y && g'.Facing = (turn g).Facing)
       
    let move g =
        let x, y =
            match g.Facing with
            | North -> g.X, g.Y - 1
            | East -> g.X + 1, g.Y
            | South -> g.X, g.Y + 1
            | West -> g.X - 1, g.Y

        match Set.contains (x, y) obstacles with
        | true -> turn g
        | false ->
            match checkForLoop g with
            | false -> { g with X = x; Y = y }
            | true -> {g with X = x; Y = y; Loops = g.Loops @ [x,y]}

    let rec patrol g =
        let g' = move g

        match g'.X, g'.Y with
        | x, y when x < 0 || y < 0 || x >= maxx || y >= maxy -> g
        | _ -> patrol { g' with Visited = g.Visited @ [ g' ] }

    patrol guard

let getVisitedCoords g = g.Visited |> List.map (fun v -> v.X, v.Y)

let part1 fn () =
    let guard = solve fn
    getVisitedCoords guard |> Set.ofSeq |> Set.count |> int64

let part2 fn () =
    let guard = solve fn
    printfn "%A" guard.Loops
    guard.Loops |>List.length |> int64
