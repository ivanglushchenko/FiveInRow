module FiveInRow.Core.Position

open GameDef
open FiveInRow.Core
open Row
open RowHistogram
open Threat
open SquareX
open PersistentHashMap
open FSharpx.Collections

type Position = 
    {
        Moves: PersistentHashMap<Point, Player>
        Threats: PersistentHashMap<int, PersistentHashMap<Player, ThreatX>>
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

let enumerateSequencesConstrained fromPoint toPoint nextPoint length minOccupied moves =
    let occupied = function | InBoardSquare (s, _) when isOccupied s -> 1 | _ -> 0
    let get p = if PersistentHashMap.containsKey p moves then Occupied moves.[p] else Empty
    let rec loop p s occupiedCount =
        seq {
            let element = if isInBounds p then InBoardSquare (get p, p) else Border
            let new_s, new_occupiedCount = 
                let extended = Deque.conj element s 
                if extended.Length > length then 
                    let removedItem, shortened = extended.Uncons
                    shortened, occupiedCount + occupied element - occupied removedItem
                else extended, occupiedCount + occupied element
            if Deque.length new_s = length && new_occupiedCount >= minOccupied then yield new_s |> Deque.toSeq |> Seq.toList
            if p <> toPoint then
                yield! loop (nextPoint p) new_s new_occupiedCount }
    loop fromPoint Deque.empty 0

let enumerateSequencesForPoint p dir moves =
    let rec step dp c p = if c = 0 || isInBounds p = false then p else step dp (c - 1) (dp p)
    let extend dir p = step (getDec dir) boardDimension p, step (getInc dir) boardDimension p
    seq {
        let pmin, pmax = extend dir p
        let smin, smax = extend S p
        let semin, semax = extend SE p
        let swmin, swmax = extend SW p
        for l in 5..7 do
            yield! enumerateSequencesConstrained pmin pmax (getInc dir) l 2 moves }

let enumerateSequences position =
    seq {
        for t in 0..boardDimension - 1 do
            for l in 5..7 do
                yield! enumerateSequencesConstrained (t, -1) (t, boardDimension) (getInc E) l 2 position.Moves
                yield! enumerateSequencesConstrained (-1, t) (boardDimension, t) (getInc S) l 2 position.Moves
        for t in 0..boardDimension - minPatternLength do
            for l in 5..7 do
                yield! enumerateSequencesConstrained (-1, t - 1) (boardDimension - t, boardDimension) (getInc SE) l 2 position.Moves
                yield! enumerateSequencesConstrained (-1, boardDimension - t) (boardDimension - t, -1) (getInc SW) l 2 position.Moves
                if t <> 0 then
                    yield! enumerateSequencesConstrained (t - 1, -1) (boardDimension, boardDimension - t) (getInc SE) l 2 position.Moves
                    yield! enumerateSequencesConstrained (t - 1, boardDimension) (boardDimension, t - 1) (getInc SW) l 2 position.Moves }

let getThreatRowIndex dir p =
    match dir with
    | S -> snd p
    | E -> fst p
    | SE -> snd p - fst p
    | SW -> snd p + fst p

let extendConstrained threatOwner p player position =
    if position.Moves.ContainsKey p then failwith "Cell is occupied already"

    let newMoves = position.Moves.Add (p, player)

    let allSequences = 
        seq {
            yield S, enumerateSequencesForPoint p S newMoves
            yield E, enumerateSequencesForPoint p E newMoves
            yield SE, enumerateSequencesForPoint p SE newMoves
            yield SW, enumerateSequencesForPoint p SW newMoves }

    let updateThreatsForPlayer player threats =
        let updateThreats acc (dir, sequences) =
            let i = getThreatRowIndex dir p
            let threats = matchThreats player sequences |> Seq.toList
            let playerThreats = if PersistentHashMap.containsKey i acc then acc.[i] else PersistentHashMap.empty
            let pointThreats = if PersistentHashMap.containsKey player playerThreats then playerThreats.[player] else SquareX.empty
            let newPointThreats = SquareX.update dir (Some threats) pointThreats
            let upsert k v map =
                if PersistentHashMap.containsKey k map = false then PersistentHashMap.add k v map
                else map.Remove k |> PersistentHashMap.add k v
            let newPlayerThreats = upsert player newPointThreats playerThreats 
            upsert i newPlayerThreats acc
        allSequences |> Seq.fold updateThreats threats
    let newThreats = 
        match threatOwner with
        | Some p -> position.Threats |> updateThreatsForPlayer p
        | _ -> position.Threats |> updateThreatsForPlayer Player1 |> updateThreatsForPlayer Player2
    {
        Moves = newMoves
        Threats = newThreats
    }

let extend = extendConstrained None

let replay moves position =
    List.fold (fun (acc, player) p -> extend p player acc, next player) (position, Player1) moves

let replayForPlayer moves player position =
    List.fold (fun acc p -> extend p player acc) position moves

let identifyThreats player position =
    position 
        |> enumerateSequences 
        |> matchThreats player

let getThreats player position =
    let extractThreats tx = 
        if PersistentHashMap.containsKey player tx then SquareX.toSeq tx.[player] |> Seq.choose id else Seq.empty
    position.Threats |> Seq.collect (snd >> extractThreats) |> Seq.collect id

let findThreat player kind position =
    getThreats player position |> Seq.tryPick (fun (k, v) -> if k = kind then Some v else None)
