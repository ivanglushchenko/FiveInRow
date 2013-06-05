module FiveInRow.Foundation.GameAI

open GameDef
open System


type AI =
    abstract member Moves: (CellPos * float) list with get
    abstract member Winner: Player option with get


type BaseAI(board: Board) = 
    abstract member CreateAI: Board -> BaseAI

    default x.CreateAI board = raise (Exception())

    abstract RowScores: float[] with get

    default x.RowScores with get() = raise (Exception()) 

    abstract CombineProbabilities: float -> float -> float

    default x.CombineProbabilities _ _ = raise (Exception())

    abstract member Fitness: Map<Player, Fitness> with get

    abstract PossibleBoards: (Cell * Fitness * Fitness) list with get

    default x.PossibleBoards
        with get() = 
            let possibleMoves = 
                seq { for cell in board.Cells do
                        if cell.IsEmpty then
                            if Cell.Neighbours cell board.CellsMap |> Seq.exists (fun c -> c.IsEmpty = false) then
                                yield cell }
            let opponent = next board.Player
            let getFitness pos player = x.CreateAI(board.SetAs pos player |> Option.get).Fitness |> Map.find player
            possibleMoves |> Seq.map (fun c -> (c, getFitness c.Pos board.Player, getFitness c.Pos opponent))  |> Seq.toList

    default x.Fitness
        with get() =
            let inline score length rank =
                match length, rank with
                | l, _ when l >= 5 -> x.RowScores.[0]
                | 4, 2 -> x.RowScores.[1]
                | 4, 1 -> x.RowScores.[2]
                | 4, 0 -> x.RowScores.[3]
                | 3, 2 -> x.RowScores.[4]
                | 3, 1 -> x.RowScores.[5]
                | 3, 0 -> x.RowScores.[6]
                | _ -> rank * rank |> float

            let calcFitness player = 
                let rowStats =
                    let array = Array.init 6 (fun i -> Array.create 3 0)
                    for rowGroup in board.RowsMap.[player] do
                        for row in rowGroup.Value do
                            if row.Length <= 5 then
                                let trimmedLength = min row.Length 5
                                array.[trimmedLength].[row.Rank] <- array.[trimmedLength].[row.Rank] + 1
                    array
                let numOfRows length rank = rowStats.[length].[rank]
                if rowStats.[5] |> Array.exists (fun n -> n > 0) then Win
                else if numOfRows 4 2 >= 1 then WinIn1Turn
                else if numOfRows 4 1 >= 2 then WinIn2Turns
                else if numOfRows 3 2 >= 2 then WinIn2Turns
                else if numOfRows 3 2 >= 1 && numOfRows 4 1 >= 1 then WinIn2Turns
                else 
                    let sumRanks length ranks = ranks |> Array.mapi (fun rank count -> (float count) * score length rank) |> Array.sum
                    Probability(rowStats |> Array.mapi sumRanks |> Array.sum)
            Map.ofList [(Player1, calcFitness Player1); (Player2, calcFitness Player2)]

    interface AI with
        member x.Moves 
            with get() =
                let possibleBoards = x.PossibleBoards

                let myWins = possibleBoards |> List.filter (fun (v, f1, f2) -> match f1 with | Probability _ -> false | _ -> true) |> List.map (fun (v, f1, f2) -> f1) |> Set.ofList
                let opWins = possibleBoards |> List.filter (fun (v, f1, f2) -> match f2 with | Probability _ -> false | _ -> true) |> List.map (fun (v, f1, f2) -> f2) |> Set.ofList

                let noMyWin = myWins.Contains Win = false
                let noOpWin = opWins.Contains Win = false
                let noWin = noMyWin && noOpWin
                let noMyWinIn1Turn = myWins.Contains WinIn1Turn = false
                let noOpWinIn1Turn = opWins.Contains WinIn1Turn = false
                let noWinIn1Turn = noMyWinIn1Turn && noOpWinIn1Turn
                let noMyWinIn2Turns = myWins.Contains WinIn2Turns = false
                let noOpWinIn2Turns = opWins.Contains WinIn2Turns = false
                let noWinIn2Turns = noMyWinIn2Turns && noOpWinIn2Turns

                let combine f1 f2 =
                    match (f1, f2) with
                    | (Win, _)                                                                     -> Double.PositiveInfinity
                    | (_, Win)                       when noMyWin                                  -> Double.PositiveInfinity
                    | (WinIn1Turn, _)                when noWin                                    -> Double.PositiveInfinity
                    | (_, WinIn1Turn)                when noWin && noMyWinIn1Turn                  -> Double.PositiveInfinity
                    | (WinIn2Turns, _)               when noWin && noWinIn1Turn                    -> Double.PositiveInfinity
                    | (_, WinIn2Turns)               when noWin && noWinIn1Turn && noMyWinIn2Turns -> Double.PositiveInfinity
                    | _ -> x.CombineProbabilities (match f1 with | Probability p -> p | _ -> 0.0) (match f2 with | Probability p -> p | _ -> 0.0)

                if possibleBoards.IsEmpty then
                    let center = (boardDimension / 2 + 1, boardDimension / 2 + 1)
                    if board.CellsMap.[fst center].[snd center].IsEmpty then [ (center, 0.0) ]
                    else if board.Cells |> Seq.isEmpty then []
                    else [ ((board.Cells |> Seq.head).Pos, 0.0) ]
                else [ for (cell, f1, f2) in possibleBoards -> (cell.Pos, combine f1 f2) ] |> List.sortBy (fun (k, v) -> -v)

        member x.Winner
            with get() = 
                match x.Fitness |> Map.toList |> List.filter (fun (p, f) -> f = Win) |> List.map fst with
                | hd :: tl -> Some(hd)
                | [] -> None


type EasyAI(board) =
    inherit BaseAI(board)

    override x.CreateAI board = EasyAI(board) :> BaseAI

    override x.RowScores 
        with get() = 
            [|  Int32.MaxValue |> float; // length 5
                64.0;                    // length 4, rank 2
                64.0;                    // length 4, rank 1
                8.0;                     // length 4, rank 0
                64.0;                    // length 3, rank 2
                64.0;                    // length 3, rank 1
                8.0                      // length 3, rank 0
            |]

    override x.CombineProbabilities p1 p2 = p1 + p2


type MediumAI(board) =
    inherit BaseAI(board)

    override x.CreateAI board = MediumAI(board) :> BaseAI

    override x.RowScores 
        with get() = 
            [|  Int32.MaxValue |> float; // length 5
                1000000.0;               // length 4, rank 2
                100.0;                   // length 4, rank 1
                1.0;                     // length 4, rank 0
                10000.0;                 // length 3, rank 2
                10.0;                    // length 3, rank 1
                1.0                      // length 3, rank 0
            |]

    override x.CombineProbabilities p1 p2 = p1 + p2 / 2.0


type HardAI(board) =
    inherit MediumAI(board)

    override x.CreateAI board = HardAI(board) :> BaseAI

    override x.PossibleBoards
        with get() = 
            let possibleMoves = 
                seq { for cell in board.Cells do
                        if cell.IsEmpty then
                            if Cell.K_Neighbours cell board.CellsMap 2 |> Seq.exists (fun c -> c.IsEmpty = false) then
                                yield cell }
            let opponent = next board.Player
            let getFitness pos player = x.CreateAI(board.SetAs pos player |> Option.get).Fitness |> Map.find player
            possibleMoves |> Seq.map (fun c -> (c, getFitness c.Pos board.Player, getFitness c.Pos opponent))  |> Seq.toList