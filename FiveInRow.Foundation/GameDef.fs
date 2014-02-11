﻿module FiveInRow.Foundation.GameDef

// Types
type Player = 
    | Player1 
    | Player2

    override x.ToString() =
        match x with
        | Player1 -> "x"
        | Player2 -> "o" 

type CellValue = Empty | Occupied of Player

type CellPos = int * int

type Move = CellPos * Player

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

type RowKey = Direction * int

type RowId = {
    direction: Direction;
    zero: int;
    startPoint: int;
    endPoint: int }

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
        match x with
        | Mate(p, t) -> sprintf "%s M %i" (p.ToString()) t
        | Check(p, t) -> sprintf "%s C %i" (p.ToString()) t
        | InProgress(p, t) -> sprintf "%s   %s" (p.ToString()) (t.ToString("f0"))

    member x.Inc turns =
        match x with
        | Mate(p, t) -> Mate(p, t + turns)
        | Check(p, t) -> Check(p, t + turns)
        | InProgress(p, t) -> InProgress(p, t)

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

// Util functions and state
let mutable boardDimension = 51

let isValid pos = fst pos > 0 && snd pos > 0 && fst pos <= boardDimension && snd pos <= boardDimension

let next = function | Player1 -> Player2 | _ -> Player1

let neighbours (c1: CellPos) (c2: CellPos) = abs(fst c1 - fst c2) <= 1 && abs(snd c1 - snd c2) <= 1

let compareStatus (bs1: BoardStatus) (bs2: BoardStatus) =
    match (bs1, bs2) with
    | (Mate(_, t1), Mate(_, t2)) -> t1.CompareTo t2
    | (Mate(_, _), _) -> -1
    | (_, Mate(_, _)) -> 1
    | (Check(_, t1), Check(_, t2)) -> t1.CompareTo t2
    | (Check(_, _), _) -> -1
    | (_, Check(_, _)) -> 1
    | (InProgress(_, t1), InProgress(_, t2)) -> -(t1.CompareTo t2)

let rnd =
    let r = new System.Random()
    fun max -> r.Next max

let allRowsForPlayer1 = System.Collections.Generic.Dictionary<RowId, int>()
let allRowsForPlayer2 = System.Collections.Generic.Dictionary<RowId, int>()

let toRowIndex (player: Player) (rowId: RowId) =
    let (ht, sign) = if player = Player1 then (allRowsForPlayer1, 1) else (allRowsForPlayer2, -1)
    if ht.ContainsKey rowId then ht.[rowId]
    else
        let nextIndex = sign * ht.Count
        ht.Add(rowId, nextIndex)
        nextIndex