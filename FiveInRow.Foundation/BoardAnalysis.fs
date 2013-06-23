module FiveInRow.Foundation.BoardAnalysis

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