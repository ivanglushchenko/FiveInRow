module FiveInRow.GameMechanics.GameDef

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

let inline isValid (r, c) = r >= 0 && c >= 0 && r < boardDimension && c < boardDimension

let inline next p = match p with | Player1 -> Player2 | _ -> Player1