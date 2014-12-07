module FiveInRow.Core.Position

open GameDef
open FiveInRow.Core
open Row
open RowHistogram
open Threat
open PersistentHashMap
open FSharpx.Collections

type ThreatX = SquareX.SquareX<Threat>

type Position = 
    {
        Moves: PersistentHashMap<Point, Player>
        Threats: PersistentHashMap<Point, ThreatX>        
    }

let empty = 
    {
        Moves = PersistentHashMap.empty
        Threats = PersistentHashMap.empty
    }

let get p position =
    if position.Moves.ContainsKey p then Occupied position.Moves.[p]
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

let step dir count point =
    let inc_p = getInc dir
    let rec loop p c =
        if c = 0 then p else loop (inc_p p) (c - 1)
    loop point count

let enumerateSequencesConstrained fromPoint toPoint nextPoint length minOccupied position =
    let occupied = function | InBoardSquare (s, _) when isOccupied s -> 1 | _ -> 0
    let rec loop p s occupiedCount =
        seq {
            let element = if isInBounds p then InBoardSquare (get p position, p) else Border
            let new_s, new_occupiedCount = 
                let extended = Deque.conj element s 
                if extended.Length > length then 
                    let removedItem, shortened = extended.Uncons
                    shortened, occupiedCount + occupied element - occupied removedItem
                else extended, occupiedCount + occupied element
            if Deque.length new_s = length && new_occupiedCount >= minOccupied then yield new_s |> Deque.toSeq
            if p <> toPoint then
                yield! loop (nextPoint p) new_s new_occupiedCount }
    loop fromPoint Deque.empty 0

let enumerateSequences position =
    seq {
        for t in 0..boardDimension - 1 do
            for l in 5..7 do
                yield! enumerateSequencesConstrained (t, -1) (t, boardDimension) (getInc E) l 2 position
                yield! enumerateSequencesConstrained (-1, t) (boardDimension, t) (getInc S) l 2 position
        for t in 0..boardDimension - minPatternLength do
            for l in 5..7 do
                yield! enumerateSequencesConstrained (-1, t - 1) (boardDimension - t, boardDimension) (getInc SE) l 2 position
                yield! enumerateSequencesConstrained (-1, boardDimension - t) (boardDimension - t, -1) (getInc SW) l 2 position
                if t <> 0 then
                    yield! enumerateSequencesConstrained (t - 1, -1) (boardDimension, boardDimension - t) (getInc SE) l 2 position
                    yield! enumerateSequencesConstrained (t - 1, boardDimension) (boardDimension, t - 1) (getInc SW) l 2 position }

let extend (r, c) player position =
    if position.Moves.ContainsKey (r, c) then failwith "Cell is occupied already"

    let newMoves = position.Moves.Add ((r, c), player)
    let newThreats = position.Threats

    //let allSequences = enumerateSequencesForPoint (r, c)

    {
        Moves = newMoves
        Threats = newThreats
    }

let replay moves position =
    List.fold (fun (acc, player) p -> extend p player acc, next player) (position, Player1) moves

let replayForPlayer moves player position =
    List.fold (fun acc p -> extend p player acc) position moves