module FiveInRow.Foundation.GameAI

open GameDef
open BoardAnalysis
open System
open FiveInRow.Core.GameDef


type AI =
    abstract member Moves: (Point * float) list with get
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


type Node(pos: Point, p1: (Board * BoardStatus), p2: (Board * BoardStatus), children: Node list, stats: StatusStatistics) =
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
        stats |> adjustBoardStats (snd p1) |> adjustBoardStats (snd p2)

    member x.Expand player radius =
        let startingBoard = (function | Player1 -> p1 | _ -> p2) player |> fst
        let children = startingBoard.Candidates |> List.map (fun pos -> Node.New startingBoard pos)
        let stats = children |> List.fold (fun acc (node: Node) -> node.AdjustStats acc) EmptyStats
        Node(pos, p1, p2, children, stats)

    static member New (startingBoard: Board) (pos: Point) =
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
            let inline scoreOf (rd: RowDistribution) =
                (float rd.L5) * x.RowScores.[0] + 
                (float rd.L4R2) * x.RowScores.[1] + 
                (float rd.L4R1) * x.RowScores.[2] +
                (float rd.L3R2) * x.RowScores.[4] +
                (float rd.L3R1) * x.RowScores.[5] +
                (float rd.L2R2) * 2.0 +
                (float rd.L2R1) * 1.0
            boardStatusOf board scoreOf

    interface AI with 
        member x.Moves 
            with get() =
                let currPlayer = board.Player
                let nextPlayer = next board.Player
                
                let cache = TurnCache()
                let currPlayerAnalysis = Turn(cache, None, board, None, currPlayer, 7).Realize()
                let nextPlayerAnalysis = Turn(cache, None, board.SwitchPlayer(), None, nextPlayer, 7).Realize()

                //let tree = currPlayerAnalysis.ToStringTree()
                //System.Diagnostics.Debug.WriteLine tree

                let l1 = currPlayerAnalysis.Boards.Length
                //let s1 = currPlayerAnalysis.Boards |> List.map (fun b -> b.GroupId) |> Set.ofList// 4461
                let s3 = currPlayerAnalysis.Boards |> List.map (fun b -> b.Hash) |> Set.ofList
                //let s2 = currPlayerAnalysis.Boards |> Seq.groupBy (fun b -> b.GroupId) |> Seq.toList
//                for (groupId, boards) in s2 do
//                    let distinctIds = boards |> Seq.distinctBy (fun b -> b.Hash) |> Seq.toList
//                    if distinctIds.Length > 1 then
//                        for b in distinctIds do
//                            let s1 = b.Print()
//                            System.Diagnostics.Debug.WriteLine("b:")
//                            System.Diagnostics.Debug.WriteLine(s1)
                match (snd currPlayerAnalysis.Result, snd nextPlayerAnalysis.Result) with
                | (Some(Mate(p1, t1)), Some(Mate(p2, t2))) -> if t1 < t2 then [ (fst currPlayerAnalysis.Result, 1.0) ] else [ (fst nextPlayerAnalysis.Result, 1.0) ]
                | (Some(Mate(p1, t1)), _) -> [ (fst currPlayerAnalysis.Result, 1.0) ]
                | (_, Some(Mate(p2, t2))) -> [ (fst nextPlayerAnalysis.Result, 1.0) ]
                | _ ->
                    let allNodes = board.Candidates |> List.map (fun pos -> Node.New board pos)
                    let notLoosingNodes = 
                        allNodes 
                        |> List.filter (fun node -> match node.StatusFor currPlayer with | Mate(p, _) when p <> currPlayer -> false | _ -> true)
                        |> List.sortWith (fun n1 n2 -> compareStatus (n1.CombinedStatusFor currPlayer) (n2.CombinedStatusFor currPlayer))
                    let topMoves =
                        if notLoosingNodes.Length < 2 then notLoosingNodes
                        else
                            let topBunch () = notLoosingNodes.Head :: notLoosingNodes.Tail |> Seq.takeWhile (fun node -> 0 = compareStatus node.TopStatus notLoosingNodes.Head.TopStatus) |> Seq.toList
                            match notLoosingNodes.Head.TopStatus with
                            | Mate(_, _) -> topBunch ()
                            | Check(_, _) ->
                                let isCheck player (node: Node) = match node.StatusFor player with | Check(_, _) -> true | _ -> false
                                let myChecks = notLoosingNodes |> List.filter (isCheck currPlayer)
                                if myChecks.IsEmpty then topBunch()
                                else
                                    let opChecks = myChecks |> List.filter (isCheck nextPlayer)
                                    if opChecks.IsEmpty then myChecks else opChecks
                            | _ -> notLoosingNodes |> Seq.take 5 |> Seq.toList

                    match topMoves with
                    | hd :: [] -> [(hd.Pos, 1.0)]
                    | hd :: tl -> 
                        match hd.StatusFor currPlayer with
                        | Mate(p, 0) when p = currPlayer ->  [(hd.Pos, 1.0)]
                        | _ ->
                            let expandedNodes = topMoves |> List.map (fun node -> node.Expand currPlayer 2) |> List.sortBy (fun node -> node.Stats)
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
                                            if currPlayer = Player1 then (s.Stats.Player1Checks, -s.Stats.Player1MaxFitness - s.Stats.Player2MaxFitness / 2.0) 
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