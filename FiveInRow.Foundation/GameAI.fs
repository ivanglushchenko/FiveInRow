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

    abstract PossibleBoards: Board -> (Cell * Fitness * Fitness) list

    default x.PossibleBoards board =
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
                let possibleBoards = x.PossibleBoards board

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

    override x.PossibleBoards board =
        let possibleMoves = 
            seq { for cell in board.Cells do
                    if cell.IsEmpty then
                        if Cell.K_Neighbours cell board.CellsMap 2 |> Seq.exists (fun c -> c.IsEmpty = false) then
                            yield cell }
        let opponent = next board.Player
        let getFitness pos player = x.CreateAI(board.SetAs pos player |> Option.get).Fitness |> Map.find player
        possibleMoves |> Seq.map (fun c -> (c, getFitness c.Pos board.Player, getFitness c.Pos opponent))  |> Seq.toList

    override x.CombineProbabilities p1 p2 = p1 + p2 / 2.0


type Node = { value: Cell; ai: AI } 
    

type HardAI(board) =
    inherit EasyAI(board)

    override x.CreateAI board = HardAI(board) :> BaseAI

    override x.Equals that = base.Equals that

    override x.GetHashCode() = base.GetHashCode()

    override x.ToString() = x.BoardStatus.ToString()

    interface IComparable with
        member x.CompareTo other =
            match (x.BoardStatus, (other :?> HardAI).BoardStatus) with
            | (Mate(_, t1), Mate(_, t2)) -> t1.CompareTo t2
            | (Mate(_, _), _) -> -1
            | (_, Mate(_, _)) -> 1
            | (Check(_, t1), Check(_, t2)) -> t1.CompareTo t2
            | (Check(_, _), _) -> -1
            | (_, Check(_, _)) -> 1
            | (InProgress(_, t1), InProgress(_, t2)) -> (t1.CompareTo t2)

    member x.Board with get() = board

    member x.BoardStatus
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

            let rowStats =
                let array = Array.init 2 (fun _ -> Array.init 6 (fun i -> Array.create 3 0))
                for rowGroup in board.RowsMap do
                    for rows in rowGroup.Value do
                        for row in rows.Value do
                        if row.Length <= 5 then
                            let playerPos = if board.Player = rowGroup.Key then 0 else 1
                            let trimmedLength = min row.Length 5
                            array.[playerPos].[trimmedLength].[row.Rank] <- array.[playerPos].[trimmedLength].[row.Rank] + 1
                array
            let numOfRows pi length rank = rowStats.[pi].[length].[rank]
            let myRows = numOfRows 0
            let opRows = numOfRows 1
            let opponent = board.Player |> next
            if myRows 5 2 > 0 || myRows 5 1 > 0 || myRows 5 0 > 0 then raise (Exception("Game should have stopped 2 rounds ago"))
            else if opRows 5 2 > 0 || opRows 5 1 > 0 || opRows 5 0 > 0 then Mate(opponent, 0)
            else if myRows 4 2 > 0 || myRows 4 1 > 0 then Mate(board.Player, 1)
            else if opRows 4 2 > 0 || opRows 4 1 > 1 then Mate(opponent, 2)
            else if myRows 3 2 > 0 then Mate(board.Player, 3)
            else if opRows 3 2 > 1 || (opRows 3 2 > 0 && opRows 4 1 > 0) then Mate(opponent, 4)
            else if opRows 3 2 > 0 then Check(opponent, 4)
            else if opRows 4 1 > 0 then Check(opponent, 2)
            else 
                let sumRanks length ranks = ranks |> Array.mapi (fun rank count -> (float count) * score length rank) |> Array.sum
                let sumLengths lengths = lengths |> Array.mapi sumRanks |> Array.sum
                // The purpose is to estimate how last move affected player's positions. It means we are more interested in board.Player's opponent score.8
                InProgress(opponent, 0.01 + sumLengths rowStats.[1])

    interface AI with
        member x.Moves 
            with get() = 
                let targetPlayer = board.Player
                let adversary = next board.Player

                let nextAs (board: Board) (player: Player) =
                    seq { for cell in board.Cells do
                            if cell.IsEmpty then
                                if Cell.K_Neighbours cell board.CellsMap 1 |> Seq.exists (fun c -> c.IsEmpty = false) then
                                    let nextBoard = board.SetAs cell.Pos player
                                    if nextBoard.IsSome then
                                        let ai = HardAI(nextBoard.Value)
                                        yield (cell.Pos, ai) } |> Seq.toList
                let next (board: Board) = nextAs board board.Player
                let addTestPos x y = ((x, y), HardAI(board.SetAs (x, y) board.Player |> Option.get))
                let nextBoards = next board
                let nextBoards = [ addTestPos 7 12; addTestPos 13 13 ]

                let toNum status =
                    match status with
                    | Mate(p, t) -> (if p = board.Player then 1.0 else -1.0) * (10.0 - (float t)) * 1000000.0
                    | Check(p, t) -> (if p = board.Player then 1.0 else -1.0) * (10.0 - (float t)) * 10000.0
                    | InProgress(_, p) -> (p * 1000.0 |> int |> float) / 1000.0

                let incTurns player turns = if player = targetPlayer then turns + 2 else turns + 1
                let analyzeGameTree (start: HardAI) =
                    match start.BoardStatus with
                    | Mate(_, 0) -> start.BoardStatus
                    | _ ->
                        let myPossibilities = nextAs start.Board targetPlayer |> List.sortBy snd |> List.map (fun (p, ai) -> (p, ai.BoardStatus))
                        let opPossibilities = nextAs start.Board adversary |> List.sortBy snd |> List.map (fun (p, ai) -> (p, ai.BoardStatus))
                        match (snd myPossibilities.Head, snd opPossibilities.Head) with
                        | (Mate(p1, t1), Mate(p2, t2)) -> if incTurns p1 t1 < incTurns p2 t2 then Mate(p1, t1 + 2) else Mate(p2, t2 + 1)
                        | (Mate(p1, t1), _) -> Mate(p1, t1 + 2)
                        | (_, Mate(p, t)) -> Mate(p, t + 1)
                        | (Check(p1, t1), Check(p2, t2)) -> if incTurns p1 t1 < incTurns p2 t2 then Check(p1, t1 + 2) else Check(p2, t2 + 1)
                        | (Check(p1, t1), _) -> Check(p1, t1 + 2)
                        | (_, Check(p, t)) -> Check(p, t + 1)
                        | _ ->
                            let analyzePossibilities (xs: (CellPos * BoardStatus) list) =
                                let acc = Array.create 3 0.0
                                for (_, bs) in xs do
                                    match bs with
                                    | Mate(_, _) -> acc.[0] <- acc.[0] + 1.0
                                    | Check(_, _) -> acc.[1] <- acc.[1] + 1.0
                                    | InProgress(_, p) -> if acc.[2] < p then acc.[2] <- p
                                acc
                            let myAnalysis = analyzePossibilities myPossibilities
                            InProgress(start.Board.Player, toNum start.BoardStatus * myAnalysis.[1])

//                let combinedBoads = 
//                    nextBoards 
//                    |> List.map (fun (pos, board) -> async { return (pos, analyzeGameTree board) }) 
//                    |> List.toArray 
//                    |> Async.Parallel 
//                    |> Async.RunSynchronously 
//                    |> Array.toList
                let combinedBoads = nextBoards |> List.map (fun (pos, board) -> (pos, analyzeGameTree board))
                let moves = combinedBoads |> List.map (fun (p, b) -> (p, toNum b)) |> List.sortBy snd |> List.rev
                if moves.IsEmpty then
                    let center = (boardDimension / 2 + 1, boardDimension / 2 + 1)
                    if board.CellsMap.[fst center].[snd center].IsEmpty then [ (center, 0.0) ]
                    else if board.Cells |> Seq.isEmpty then []
                    else [ ((board.Cells |> Seq.head).Pos, 0.0) ]
                else
                    moves |> Seq.take 1 |> Seq.toList

        member x.Winner
            with get() =
                match x.BoardStatus with
                | Mate(p, 0) -> Some(p)
                | _ -> None