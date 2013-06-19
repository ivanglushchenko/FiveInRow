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
                        if Cell.Neighbours cell.Pos board.CellsMap |> Seq.exists (fun c -> c.IsEmpty = false) then
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

    override x.Fitness
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
                else 
                    let sumRanks length ranks = ranks |> Array.mapi (fun rank count -> (float count) * score length rank) |> Array.sum
                    Probability(rowStats |> Array.mapi sumRanks |> Array.sum)
            Map.ofList [(Player1, calcFitness Player1); (Player2, calcFitness Player2)]

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
                        if Cell.K_Neighbours cell.Pos board.CellsMap 2 |> Seq.exists (fun c -> c.IsEmpty = false) then
                            yield cell }
        let opponent = next board.Player
        let getFitness pos player = x.CreateAI(board.SetAs pos player |> Option.get).Fitness |> Map.find player
        possibleMoves |> Seq.map (fun c -> (c, getFitness c.Pos board.Player, getFitness c.Pos opponent))  |> Seq.toList

    override x.CombineProbabilities p1 p2 = p1 + p2 / 2.0


type StatusStatistics = 
    {
        Mate: (Player * int) option
        Player1Checks: int
        Player2Checks: int
        Player1MaxFitness: float
        Player2MaxFitness: float
    }

let EmptyStats = { Mate = None; Player1Checks = 0; Player2Checks = 0; Player1MaxFitness = 0.0; Player2MaxFitness = 0.0 }


type Turn(startingBoard) =
    let q = 0

and Node(pos: CellPos, p1: (Board * BoardStatus), p2: (Board * BoardStatus), children: Node list, stats: StatusStatistics) =
    let cmp = compareStatus (snd p1) (snd p2)

    member x.Pos with get() = pos

    member x.Children with get() = children

    member x.Stats with get() = stats

    member x.StatusFor player =
        match player with
        | Player1 -> snd p1
        | Player2 -> snd p2

    member x.TopStatus
        with get() =
            match cmp with
            | -1 -> snd p1
            | _ -> snd p2

    member x.CombinedStatusFor player = 
        match (snd p1, snd p2) with
        | (InProgress(p1, t1), InProgress(p2, t2)) -> InProgress(player, if player = Player1 then t1 + t2 / 2.0 else t1 / 2.0 + t2)
        | _ -> x.TopStatus

    member x.AdjustStats (stats: StatusStatistics) =
        let addStatus status stats =
            match status with
            | Mate(p1, t1) ->
                match stats.Mate with
                | Some(p2, t2) when t1 > t2 -> stats
                | _ -> { Mate = Some(p1, t1); Player1Checks = stats.Player1Checks; Player2Checks = stats.Player2Checks; Player1MaxFitness = stats.Player1MaxFitness; Player2MaxFitness = stats.Player2MaxFitness }
            | Check(Player1, t) -> { Mate = stats.Mate; Player1Checks = stats.Player1Checks + 1; Player2Checks = stats.Player2Checks; Player1MaxFitness = stats.Player1MaxFitness; Player2MaxFitness = stats.Player2MaxFitness }
            | Check(Player2, t) -> { Mate = stats.Mate; Player1Checks = stats.Player1Checks; Player2Checks = stats.Player2Checks + 1; Player1MaxFitness = stats.Player1MaxFitness; Player2MaxFitness = stats.Player2MaxFitness }
            | InProgress(Player1, t) -> { Mate = stats.Mate; Player1Checks = stats.Player1Checks; Player2Checks = stats.Player2Checks; Player1MaxFitness = max t stats.Player1MaxFitness; Player2MaxFitness = stats.Player2MaxFitness }
            | InProgress(Player2, t) -> { Mate = stats.Mate; Player1Checks = stats.Player1Checks; Player2Checks = stats.Player2Checks; Player1MaxFitness = stats.Player1MaxFitness; Player2MaxFitness = max t stats.Player2MaxFitness }
        stats |> addStatus (snd p1) |> addStatus (snd p2)

    member x.Expand player radius =
        let startingBoard = (function | Player1 -> p1 | _ -> p2) player |> fst
        let children = startingBoard.Candidates |> List.map (fun pos -> Node.New startingBoard pos)
        let stats = children |> List.fold (fun acc (node: Node) -> node.AdjustStats acc) EmptyStats
        Node(pos, p1, p2, children, stats)

    static member New (startingBoard: Board) (pos: CellPos) =
        let adjustFor (player: Player) (boardStatus: BoardStatus) = 
            if player = startingBoard.Player then boardStatus else boardStatus.Inc 1
        let statusFor player = 
            match startingBoard.SetAs pos player with
            | Some(board) -> (board, HardAI(board).BoardStatus |> adjustFor player)
            | None -> raise (Exception("Bad initial pos provided"))

        Node(pos, statusFor Player1, statusFor Player2, [], EmptyStats)

    static member StartFrom (board: Board) =
        let targetPlayer = board.Player
        let adversary = next board.Player
        let allNodes = board.Candidates |> List.map (fun pos -> Node.New board pos)
        let notLoosingNodes = 
            allNodes 
            |> List.filter (fun node -> match node.StatusFor targetPlayer with | Mate(p, _) when p <> targetPlayer -> false | _ -> true)
            |> List.sortBy (fun node -> node.TopStatus)

        []

    override x.ToString() = sprintf "[%2i, %2i]  %O  |  %O" (fst pos) (snd pos) (snd p1) (snd p2)

    interface IComparable with
        member x.CompareTo other = compareStatus x.TopStatus (other :?> Node).TopStatus


and HardAI(board) =
    inherit EasyAI(board)

    override x.CreateAI board = HardAI(board) :> BaseAI

    override x.Equals that = base.Equals that

    override x.GetHashCode() = base.GetHashCode()

    override x.ToString() = x.BoardStatus.ToString()

    interface IComparable with
        member x.CompareTo other = compareStatus x.BoardStatus (other :?> HardAI).BoardStatus

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
                
                let allNodes = board.Candidates |> List.map (fun pos -> Node.New board pos)
                let notLoosingNodes = 
                    allNodes 
                    |> List.filter (fun node -> match node.StatusFor targetPlayer with | Mate(p, _) when p <> targetPlayer -> false | _ -> true)
                    |> List.sortWith (fun n1 n2 -> compareStatus (n1.CombinedStatusFor targetPlayer) (n2.CombinedStatusFor targetPlayer))
                let topMoves =
                    if notLoosingNodes.Length < 2 then notLoosingNodes
                    else
                        let topBunch () = notLoosingNodes.Head :: notLoosingNodes.Tail |> Seq.takeWhile (fun node -> 0 = compareStatus node.TopStatus notLoosingNodes.Head.TopStatus) |> Seq.toList
                        match notLoosingNodes.Head.TopStatus with
                        | Mate(_, _) -> topBunch ()
                        | Check(_, _) ->
                            let isCheck player (node: Node) = match node.StatusFor player with | Check(_, _) -> true | _ -> false
                            let myChecks = notLoosingNodes |> List.filter (isCheck targetPlayer)
                            if myChecks.IsEmpty then topBunch()
                            else
                                let opChecks = myChecks |> List.filter (isCheck adversary)
                                if opChecks.IsEmpty then myChecks else opChecks
                        | _ -> notLoosingNodes |> Seq.take 5 |> Seq.toList

                match topMoves with
                | hd :: [] -> [(hd.Pos, 1.0)]
                | hd :: tl -> 
                    match hd.StatusFor targetPlayer with
                    | Mate(p, 0) when p = targetPlayer ->  [(hd.Pos, 1.0)]
                    | _ ->
                        let expandedNodes = topMoves |> List.map (fun node -> node.Expand targetPlayer 2) |> List.sortBy (fun node -> node.Stats)
                        let notLoosingExpNodes = 
                            expandedNodes 
                            |> List.filter (
                                fun node ->
                                    match node.Stats.Mate with
                                    | Some(adversary, _) -> false
                                    | _ -> true)
                        if notLoosingExpNodes.IsEmpty then [(expandedNodes.Head.Pos, 2.0)]
                        else 
                            let sortedNodes = 
                                notLoosingExpNodes 
                                |> List.sortBy (
                                    fun s -> 
                                        if targetPlayer = Player1 then (s.Stats.Player1Checks, -s.Stats.Player1MaxFitness - s.Stats.Player2MaxFitness / 2.0) 
                                        else (s.Stats.Player2Checks, -s.Stats.Player2MaxFitness - s.Stats.Player1MaxFitness / 2.0))
                            [(sortedNodes.Head.Pos, 2.0)]
                | [] ->
                    match allNodes with
                    | _ :: _ -> [(allNodes.Head.Pos, -1.0)]
                    | _ ->
                        let center = (boardDimension / 2 + 1, boardDimension / 2 + 1)
                        if board.CellsMap.[fst center].[snd center].IsEmpty then [ (center, 0.0) ]
                        else if board.Cells |> Seq.isEmpty then []
                        else [ ((board.Cells |> Seq.head).Pos, 0.0) ]

        member x.Winner
            with get() =
                match x.BoardStatus with
                | Mate(p, 0) -> Some(p)
                | _ -> None