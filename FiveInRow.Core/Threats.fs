module FiveInRow.Core.Threats

open System.Collections.Generic
open PersistentHashMap
open FiveInRow.Core
open Threat
open GameDef
open Board

let enumerateSequences board =
    let getInBoardSquare (r, c) =
        InBoardSquare (Board.get r c board, (r, c))
    let isOccupied (r, c) = 
        Board.get r c board |> isOccupied
    let generateSeqOfIndices lengthFrom lengthTo p inc_p =
        let rec getNext p l acc breakIfOffboard numOfActiveSquares =
            seq {
                if l = lengthTo then 
                    if numOfActiveSquares > 1 then yield acc
                else
                    if l >= lengthFrom && numOfActiveSquares > 1 then yield acc
                    let incActiveCount = if isOccupied p then 1 else 0
                    if isInBounds p then
                        yield! getNext (inc_p p) (l + 1) (getInBoardSquare p :: acc) breakIfOffboard (numOfActiveSquares + incActiveCount) 
                    else
                        if breakIfOffboard = false then
                            yield! getNext (inc_p p) (l + 1) (Border :: acc) true (numOfActiveSquares + incActiveCount) }
        getNext p 0 [] false 0

    seq {
        for r in -1..boardDimension - 1 do
            for c in -1..boardDimension - 1 do
                for inc in [ (fun p -> fst p, snd p + 1)
                             (fun p -> fst p + 1, snd p)
                             (fun p -> fst p + 1, snd p + 1)
                             (fun p -> fst p + 1, snd p - 1) ] do
                    yield! generateSeqOfIndices 5 7 (r, c) inc }

let (>>=) m cont = Option.bind cont m

let identifyThreats player sequences =
    let toThreatPatternSquare = function
        | InBoardSquare (Occupied pl, p) when pl = player -> Rest, p
        | InBoardSquare (Occupied _, p) -> Obstacle, p
        | InBoardSquare (Empty, p) -> Available, p
        | Border -> Obstacle, (-1, -1)
    let convertAndMatch s = s |> List.map toThreatPatternSquare |> matchThreat
    let runIfNone cont opt = 
        match opt with
        | Some _ -> opt
        | _ -> cont()
    seq {
        for s in sequences do
            yield! convertAndMatch s }

let identifyThreatsUnconstrained player board =
    let hasEnoughOccupiedCells l s =
        let folder c el = match el with | InBoardSquare (Occupied _, _) -> c + 1 | _ -> c
        List.fold folder 0 s >= l
    board 
        |> enumerateSequences 
        |> Seq.where (hasEnoughOccupiedCells 2) 
        |> identifyThreats player

let significantSquareDistance = 2

let buildThreatsTree player board maxDepth =
    let isDependentOrClose parent child =
        match parent with
        | Some (_, data) ->
            if child.Rest |> List.exists (fun p -> p = data.Gain) then true
            else
                match getLinearDictance child.Gain data.Gain with
                | Some d -> d < significantSquareDistance
                | _ -> false
        | _ -> true
    let rec buildNextLevel board depth threat =
        let extend board threatData =
            let folder acc el = Board.extend el (next player) acc
            threatData.Cost |> List.fold folder (Board.extend threatData.Gain player board)
        if depth = maxDepth then None
        elif (match threat with | Some (kind, _) when isWinningThreat kind -> true | _ -> false) then None
        else
            let threats = identifyThreatsUnconstrained player board |> Seq.toList
            let connectedThreats = 
                threats
                |> Seq.where (snd >> isDependentOrClose threat)
                |> Seq.toList
            threats
                |> Seq.where (snd >> isDependentOrClose threat)
                |> Seq.map (
                    fun t ->
                        { Threat = t; 
                          Dependencies = buildNextLevel (extend board (snd t)) (depth + 1) (Some t) })
                |> Seq.toList
                |> Some
    buildNextLevel board 0 None

let analyzeTree tree =
    let rec countMoves node =
        match node.Threat with
        | Five, _ -> Some 0
        | StraightFour, _ -> Some 1
        | _ ->
            match node.Dependencies with
            | Some dep -> 
                let sortedWinningPaths = dep |> List.choose countMoves |> List.sortBy (fun t -> -t)
                if List.isEmpty sortedWinningPaths then None
                else Some sortedWinningPaths.Head
            | None -> None
    
    let goodNodes = 
        tree    
            |> List.map (fun n -> n, countMoves n) 
            |> List.filter (snd >> Option.isSome)
            |> List.sortBy (fun (t, c) -> -(Option.get c))
    match goodNodes with
    | (node, _) :: _ -> let (_, data) = node.Threat in Some data.Gain
    | _ -> None
