module AoC2024.Day6

open AoC2024.Prelude

type Direction =
    | North
    | East
    | South
    | West

type Guard = { Facing: Direction; X: int; Y: int; Visited: (int * int) list }

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
    | North -> {g with Facing=East}
    | East -> {g with Facing=South}
    | South -> {g with Facing=West}
    | West -> {g with Facing=North}
    
let solve fn=
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
                | '^' -> Some(Guard { X = x; Y = y; Visited = List.singleton (x,y); Facing = North })))
        |> Seq.concat
        |> Seq.choose id
        
    let guard, obstacles = extractPositions state
    
    let move g =
        let x, y =
            match g.Facing with
            | North -> g.X, g.Y - 1
            | East -> g.X + 1, g.Y
            | South -> g.X, g.Y + 1
            | West -> g.X - 1, g.Y
        match Set.contains (x,y) obstacles with
        | true -> turn g
        | false -> {g with X=x; Y=y}
        
    let rec patrol g =
        let g' = move g
        match g'.X, g'.Y with
        | x,y when x < 0 || y < 0 || x >= maxx || y >= maxy -> g
        | x,y -> patrol {g' with Visited=g.Visited @ [(x,y)]}
    patrol guard

let part1 fn () =
    let finish = solve fn
    finish.Visited |> Set.ofSeq |> Set.count |> int64
    
let part2 fn () =
    let finish = solve fn
    finish.Visited |> List.groupBy id |> List.filter (fun l -> List.length (snd l) > 1) |> printfn "%A"
    let unique = finish.Visited |> Set.ofSeq |> Set.count
    unique |> int64
