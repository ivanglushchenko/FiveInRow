﻿module FiveInRow.GameMechanics.GameDef

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

let isValid pos = fst pos > 0 && snd pos > 0 && fst pos <= boardDimension && snd pos <= boardDimension

let next = function | Player1 -> Player2 | _ -> Player1