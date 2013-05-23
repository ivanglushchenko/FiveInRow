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

// Util functions
let mutable boardDimension = 0

let isValid pos = fst pos > 0 && snd pos > 0 && fst pos <= boardDimension && snd pos <= boardDimension

let next = function | Player1 -> Player2 | _ -> Player1

let neighbours (c1: CellPos) (c2: CellPos) = abs(fst c1 - fst c2) <= 1 && abs(snd c1 - snd c2) <= 1