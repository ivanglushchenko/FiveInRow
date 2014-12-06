module FiveInRow.Core.Position

open GameDef
open FiveInRow.Core
open Row
open RowHistogram
open Threat
open PersistentHashMap

type ThreatX = SquareX.SquareX<Threat>

type Position = 
    {
        Moves: PersistentHashMap<Point, Player>
        Threats: PersistentHashMap<Point, ThreatX>        
    }

let empty dim = 
    {
        Moves = PersistentHashMap.empty
        Threats = PersistentHashMap.empty
    }

let get r c position =
    if position.Moves.ContainsKey (r, c) then Occupied position.Moves.[r, c]
    else Empty

let getRow p dir threats =
    if PersistentHashMap.containsKey p threats then SquareX.get dir threats.[p]
    else None

let setRow p dir row threats =
    if PersistentHashMap.containsKey p threats then
        let extRj = threats.[p]
        let r = threats.Remove p
        let newRj = SquareX.update dir (Some row) extRj
        r.Add (p, newRj)
    else
        threats.Add (p, SquareX.update dir (Some row) SquareX.empty)

let isInBounds (r, c) = r >= 0 && c >= 0 && r < boardDimension && c < boardDimension

let enumerateSequencesOriginatingAtPoint p inc_p position =
    let getInBoardSquare (r, c) =
        InBoardSquare (get r c position, (r, c))
    let isOccupied (r, c) = 
        get r c position |> isOccupied
    let rec getNext p l acc breakIfOffboard numOfActiveSquares =
        seq {
            if l = maxPatternLength then 
                if numOfActiveSquares > 1 then yield acc
            else
                if l >= minPatternLength && numOfActiveSquares > 1 then yield acc
                let incActiveCount = if isOccupied p then 1 else 0
                if isInBounds p then
                    yield! getNext (inc_p p) (l + 1) (getInBoardSquare p :: acc) breakIfOffboard (numOfActiveSquares + incActiveCount) 
                else
                    if breakIfOffboard = false then
                        yield! getNext (inc_p p) (l + 1) (Border :: acc) true (numOfActiveSquares + incActiveCount) }
    getNext p 0 [] false 0

let rec enumerateSequencesOriginatingAtPoints p inc_p count position =
    seq {
        if count > 0 then
            yield enumerateSequencesOriginatingAtPoint p inc_p position
            yield! enumerateSequencesOriginatingAtPoints (inc_p p) inc_p (count - 1) position }

let enumerateSequencesInDirection center dir rad position =
    let delta_r, delta_c = 
        match dir with
        | S -> 0, 1
        | E -> 1 ,0
        | SE -> 1, 1
        | SW -> -1, 1
    let inc_p (r, c) = r + delta_r, c + delta_c
    let dec_p (r, c) = r - delta_r, c - delta_c
    let rec enumIndices p dp c = 
        seq { 
            yield p
            if c > 0 then yield! enumIndices (dp p) dp (c - 1) }  
    seq {
        for p in enumIndices center dec_p rad do
            yield! enumerateSequencesOriginatingAtPoint p inc_p position
        for p in enumIndices (inc_p center) dec_p (rad - 1) do
            yield! enumerateSequencesOriginatingAtPoint p inc_p position }

let enumerateSequencesAroundPoint center rad position =
    seq {
        yield! enumerateSequencesInDirection center S rad position
        yield! enumerateSequencesInDirection center S rad position
        yield! enumerateSequencesInDirection center S rad position
        yield! enumerateSequencesInDirection center S rad position }

let enumerateSequences position =
    seq {
        for r in -1..boardDimension - 1 do
            for c in -1..boardDimension - 1 do
                for inc in [ (fun p -> fst p, snd p + 1)
                             (fun p -> fst p + 1, snd p)
                             (fun p -> fst p + 1, snd p + 1)
                             (fun p -> fst p + 1, snd p - 1) ] do
                    yield! enumerateSequencesOriginatingAtPoint (r, c) inc position }

let extend (r, c) player position =
    if position.Moves.ContainsKey (r, c) then failwith "Cell is occupied already"

    let newMoves = position.Moves.Add ((r, c), player)
    let newThreats = position.Threats

    //let allSequences = enumerateSequencesForPoint (r, c)

    {
        Moves = newMoves
        Threats = newThreats
    }

