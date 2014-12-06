module FiveInRow.Core.GameDef

type Player = 
    | Player1 
    | Player2

    override x.ToString() =
        match x with
        | Player1 -> "x"
        | Player2 -> "o"

type Point = int * int

type Move = Point * Player

type SquareStatus = 
    | Empty
    | Occupied of Player

    override x.ToString() =
        match x with
        | Empty -> "."
        | Occupied p -> p.ToString()

type Direction = 
    | S 
    | E 
    | SE 
    | SW

    override x.ToString() =
        match x with
        | S -> "S"
        | E -> "E"
        | SE -> "SE"
        | SW -> "SW"

type Difficulty = 
    | Easy 
    | Medium 
    | Hard
    | Impossible

    override x.ToString() =
        match x with
        | Easy -> "Easy"
        | Medium -> "Medium"
        | Hard -> "Hard"
        | Impossible -> "Impossible"

type OpponentType = Human | AI of Player

let mutable boardDimension = 51

let isTracingEnabled = false

let keepDeadRows = true

let inline isValid (r, c) = r >= 0 && c >= 0 && r < boardDimension && c < boardDimension

let inline next p = match p with | Player1 -> Player2 | _ -> Player1

let inline i1st (x, _, _) = x

let inline i2nd (_, x, _) = x

let inline i3rd (_, _, x) = x

let inline trd (_, _, x) = x

let inline diag x = System.Diagnostics.Debug.WriteLine x

let rand = System.Random()

let shuffle data = 
    let result = System.Collections.Generic.List<_>()
    for n in data do
        let index = rand.Next(0, result.Count)
        if index = result.Count then
            result.Add(n)
        else
            result.Add(result.[index])
            result.[index] <- n
    result :> seq<_>

let inline isEmpty v = 
    match v with
    | Empty -> true
    | _ -> false

let inline isOccupied v =
    match v with
    | Occupied _ -> true
    | _ -> false

let getLinearDictance (x1, y1) (x2, y2) =
    if x1 = x2 then abs (y1 - y2) |> Some
    elif y1 = y2 then abs (x1 - x2) |> Some
    elif abs (x1 - x2) = abs (y1 - y2) then abs (x1 - x2) |> Some
    else None