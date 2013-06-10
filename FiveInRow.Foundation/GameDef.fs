module FiveInRow.Foundation.GameDef

// Types
type Player = Player1 | Player2

type CellValue = Empty | Occupied of Player

type CellPos = int * int

type Direction = S | E | SE | SW

type RowKey = Direction * int

type Fitness = 
    | Win
    | WinIn1Turn
    | WinIn2Turns
    | Probability of float 

type BoardStatus =
    | Mate of Player * int
    | Check of Player * int
    | InProgress of Player * float

    override x.ToString() =
        let ps p =
            match p with
            | Player1 -> "X"
            | Player2 -> "O" 
        match x with
        | Mate(p, t) -> sprintf "%s M %i" (ps p) t
        | Check(p, t) -> sprintf "%s C %i" (ps p) t
        | InProgress(p, t) -> sprintf "%s ~ %f" (ps p) t

type Difficulty = Easy | Medium | Hard

type OpponentType = Human | AI of Player

// Util functions and state
let mutable boardDimension = 0

let isValid pos = fst pos > 0 && snd pos > 0 && fst pos <= boardDimension && snd pos <= boardDimension

let next = function | Player1 -> Player2 | _ -> Player1

let neighbours (c1: CellPos) (c2: CellPos) = abs(fst c1 - fst c2) <= 1 && abs(snd c1 - snd c2) <= 1