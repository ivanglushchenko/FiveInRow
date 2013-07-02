﻿module FiveInRow.Foundation.BoardAnalysis

open System
open GameDef

type RowDistribution = 
    {
        L5: int
        L4R2: int
        L4R1: int
        L3R2: int
        L3R1: int
        L2R2: int
        L2R1: int
    }

type StatusStatistics = 
    {
        Mate: (Player * int) option
        Player1Checks: int
        Player2Checks: int
        Player1MaxFitness: float
        Player2MaxFitness: float
    }

let EmptyStats = { Mate = None; Player1Checks = 0; Player2Checks = 0; Player1MaxFitness = 0.0; Player2MaxFitness = 0.0 }

type ActiveRows =
    {
        L5: Row list
    }

let rowDistributionOf (board: Board) =
    let rowStats player =
        let array = Array.init 6 (fun i -> Array.create 3 0)
        for rows in board.RowsMap.[player] do
            for row in rows.Value do
                if row.Length <= 5 then
                    let trimmedLength = min row.Length 5
                    array.[trimmedLength].[row.Rank] <- array.[trimmedLength].[row.Rank] + 1
        { L5 = array.[5].[0] + array.[5].[1] + array.[5].[2]
          L4R2 = array.[4].[2]
          L4R1 = array.[4].[1]
          L3R2 = array.[3].[2]
          L3R1 = array.[3].[1]
          L2R2 = array.[2].[2]
          L2R1 = array.[2].[1] }
    (rowStats Player1, rowStats Player2)

let boardStatusOf (board: Board) (scorer: RowDistribution -> float) =
    let (p1Distr, p2Distr) = rowDistributionOf board
    let nextPlayer = next board.Player
    let (curr, next) = if board.Player = Player1 then (p1Distr, p2Distr) else (p2Distr, p1Distr)

    if curr.L5 > 0 then raise (Exception("Game should have stopped 2 rounds ago"))
    else if next.L5 > 0 then Mate(nextPlayer, 0)
    else if curr.L4R1 > 0 || curr.L4R2 > 0 then Mate(board.Player, 1)
    else if next.L4R1 > 1 || next.L4R2 > 0 then Mate(nextPlayer, 2)
    else if curr.L3R2 > 0 then Mate(board.Player, 3)
    else if next.L3R2 > 1 || (next.L3R2 > 0 && next.L4R1 > 0) then Mate(nextPlayer, 4)
    else if next.L3R2 > 0 then Check(nextPlayer, 4)
    else if next.L4R1 > 0 then Check(nextPlayer, 2)
    else 
        // The purpose is to estimate how last move affected player's positions. It means we are more interested in board.Player's opponent score
        InProgress(nextPlayer, 0.01 + (scorer next) + (scorer curr) / 2.0)

let firstCheckOrMate (board: Board) =
    let (p1Distr, p2Distr) = rowDistributionOf board
    let nextPlayer = next board.Player
    let (curr, next) = if board.Player = Player1 then (p1Distr, p2Distr) else (p2Distr, p1Distr)

    if curr.L5 > 0 then raise (Exception("Game should have stopped 2 rounds ago"))
    else if next.L5 > 0 then Some(Mate(nextPlayer, 0))
    else if curr.L4R1 > 0 || curr.L4R2 > 0 then Some(Mate(board.Player, 1))
    else if next.L4R1 > 1 || next.L4R2 > 0 then Some(Mate(nextPlayer, 2))
    else if next.L4R1 > 0 then Some(Check(nextPlayer, 2))
    else if curr.L3R2 > 0 then Some(Mate(board.Player, 3))
    else if next.L3R2 > 1 || (next.L3R2 > 0 && next.L4R1 > 0) then Some(Mate(nextPlayer, 4))
    else if next.L3R2 > 0 then Some(Check(nextPlayer, 4))
    else None

let zeroScorer rd = 0.0

let isMateOrCheck = function
    | InProgress(_, _) -> false
    | _ -> true

let adjustBoardStats status stats =
    match status with
    | Mate(p1, t1) ->
        match stats.Mate with
        | Some(p2, t2) when t1 > t2 -> stats
        | _ -> { Mate = Some(p1, t1); Player1Checks = stats.Player1Checks; Player2Checks = stats.Player2Checks; Player1MaxFitness = stats.Player1MaxFitness; Player2MaxFitness = stats.Player2MaxFitness }
    | Check(Player1, t) -> { Mate = stats.Mate; Player1Checks = stats.Player1Checks + 1; Player2Checks = stats.Player2Checks; Player1MaxFitness = stats.Player1MaxFitness; Player2MaxFitness = stats.Player2MaxFitness }
    | Check(Player2, t) -> { Mate = stats.Mate; Player1Checks = stats.Player1Checks; Player2Checks = stats.Player2Checks + 1; Player1MaxFitness = stats.Player1MaxFitness; Player2MaxFitness = stats.Player2MaxFitness }
    | InProgress(Player1, t) -> { Mate = stats.Mate; Player1Checks = stats.Player1Checks; Player2Checks = stats.Player2Checks; Player1MaxFitness = max t stats.Player1MaxFitness; Player2MaxFitness = stats.Player2MaxFitness }
    | InProgress(Player2, t) -> { Mate = stats.Mate; Player1Checks = stats.Player1Checks; Player2Checks = stats.Player2Checks; Player1MaxFitness = stats.Player1MaxFitness; Player2MaxFitness = max t stats.Player2MaxFitness }


type Turn(board: Board, boardStatus: BoardStatus option, targetPlayer: Player, stop: int) =
    let allBoards = 
        (if stop <= 0 then [] else board.Candidates)
        |> List.choose (fun pos -> board.Set pos)
        |> List.map (fun board -> (board, firstCheckOrMate board))

    let notLoosingBoards = 
        allBoards 
        |> List.filter (fun (b, bs) -> match bs with | Some(Mate(p, _)) when p <> board.Player -> false | _ -> true)

    let winningBoards = 
        notLoosingBoards 
        |> List.filter (fun (b, bs) -> match bs with | Some(Mate(p, _)) when p = board.Player -> true | _ -> false)
        |> List.sortWith (fun s1 s2 -> compareStatus (snd s1).Value (snd s2).Value)

    let nextBoards =
        if winningBoards.Length > 0 then []
        else if notLoosingBoards.Length < allBoards.Length then notLoosingBoards
        else
            notLoosingBoards
            |> List.filter (fun (b, bs) -> match bs with | Some(InProgress(_, _)) -> false | None -> false | _ -> true)

    let nextTurns =
        if board.Player = targetPlayer then
            let rec loop l acc =
                match l with
                | (b, bs) :: tl ->
                    let turn = Turn(b, bs, targetPlayer, stop - 1)
                    match snd turn.Result with
                    | Some(Mate(p, t)) when p = targetPlayer -> turn :: acc
                    | _ -> loop tl (turn :: acc)
                | [] -> acc
            loop nextBoards []
        else 
            nextBoards 
            |> List.map (fun (b, bs) -> Turn(b, bs, targetPlayer, stop - 1))

    let sortedResults = 
        nextTurns
        |> List.map 
            (fun turn ->
                (turn, match snd turn.Result with
                        | Some(Mate(p, t)) when p = targetPlayer -> sprintf "a:%i" t
                        | Some(Mate(p, t)) when p <> targetPlayer -> sprintf "z:%i" (100 - t)
                        | Some(Check(p, t)) when p = targetPlayer -> sprintf "b:%i" t
                        | Some(Check(p, t)) when p <> targetPlayer -> sprintf "y:%i" t
                        | _ -> "k"))
        |> List.sortBy snd

    let result: (CellPos * BoardStatus option) =
        if winningBoards.Length > 0 then ((fst winningBoards.Head).LastPos, snd winningBoards.Head)
        else
            let (pos, status) = 
                if sortedResults.IsEmpty then
                    if allBoards.IsEmpty then ((-1, -1), None) else
                        let b = allBoards.Head
                        ((fst b).LastPos, snd b)
                else
                    let turn = (if board.Player = targetPlayer then sortedResults.Head else sortedResults |> List.rev |> List.head) |> fst
                    turn.Result

            match status with
            | Some(s) -> (pos, Some(s.Inc 1))
            | None -> (pos, None)

    member x.Board with get() = board

    member x.NextBoards with get() = nextBoards

    member x.WinningBoards with get() = winningBoards

    member x.SortedResults with get() = sortedResults

    member x.NextTurns with get() = nextTurns

    member x.TurnsCount 
        with get() =
            1 + (nextTurns |> List.sumBy (fun t -> t.TurnsCount))

    member x.Result
        with get(): (CellPos * BoardStatus option) = result

    override x.ToString() = sprintf "%O" board

    member x.Lines
        with get() =
            let header = sprintf "%O, %O    result: %O" board boardStatus x.Result
            let turnLines = nextTurns |> List.fold (fun acc t -> acc @ (t.Lines |> List.map (fun l -> sprintf "\t%s" l))) List.empty<string>
            header :: turnLines

    member x.ToStringTree() =
        x.Lines |> List.fold (fun (acc: System.Text.StringBuilder) s -> acc.AppendLine s) (new System.Text.StringBuilder())
