module FiveInRow.Core.GameDef

type Player = 
    | Player1 
    | Player2

    override x.ToString() =
        match x with
        | Player1 -> "x"
        | Player2 -> "o"

type Position = int * int

type Move = Position * Player

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

    override x.ToString() =
        match x with
        | Easy -> "Easy"
        | Medium -> "Medium"
        | Hard -> "Hard"

type OpponentType = Human | AI of Player

let mutable boardDimension = 51

let isTracingEnabled = false

let keepDeadRows = true

let inline isValid (r, c) = r >= 0 && c >= 0 && r < boardDimension && c < boardDimension

let inline next p = match p with | Player1 -> Player2 | _ -> Player1

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