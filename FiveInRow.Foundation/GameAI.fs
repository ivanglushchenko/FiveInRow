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
        Mate: (Player * int) option;
        Checks: int;
        MaxFitness: float
    }


type Node(pos: CellPos, p1: (Board * BoardStatus), p2: (Board * BoardStatus), children: Node list, stats: float * float * float) =
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

    member x.AdjustStats (stats: StatusStatistics) =
        match x.TopStatus with
        | Mate(p1, t1) ->
            match stats.Mate with
            | Some(p2, t2) when t1 > t2 -> stats
            | _ -> { Mate = Some(p1, t1); Checks = stats.Checks; MaxFitness = stats.MaxFitness }
        | _ -> stats

    member x.Expand player radius =
        let startingBoard = (function | Player1 -> p1 | _ -> p2) player |> fst
        //let candidates = Cell.K_Neighbours pos startingBoard.CellsMap radius |> Seq.filter (fun c -> c.IsEmpty) |> Seq.map (fun c -> c.Pos) |> Seq.toList
        let children = startingBoard.Candidates |> List.map (fun pos -> Node.New startingBoard pos)
        let stats = children |> List.fold (fun acc (node: Node) -> node.AdjustStats acc) { Mate = None; Checks = 0; MaxFitness = 0.0 }
        let analyzePossibilities (nodes: Node list) =
            let acc = Array.create 3 0.0
            let inline addStatus (op: float -> float -> float) status =
                match status with
                | Mate(_, _) -> acc.[0] <- op acc.[0] 1.0
                | Check(_, _) -> acc.[1] <- op acc.[1] 1.0
                | InProgress(_, p) -> if acc.[2] < p then acc.[2] <- p
            for node in nodes do
                player |> node.StatusFor |> addStatus (+)
                player |> next |> node.StatusFor |> addStatus (+)
            (-acc.[0], -acc.[1], -acc.[2])
        Node(pos, p1, p2, children, analyzePossibilities children)

    static member New (startingBoard: Board) (pos: CellPos) =
        let adjustFor (player: Player) (boardStatus: BoardStatus) = 
            if player = startingBoard.Player then boardStatus else boardStatus.Inc 1
        let statusFor player = 
            match startingBoard.SetAs pos player with
            | Some(board) -> (board, HardAI(board).BoardStatus |> adjustFor player)
            | None -> raise (Exception("Bad initial pos provided"))

        Node(pos, statusFor Player1, statusFor Player2, [], (0.0, 0.0, 0.0))

    override x.ToString() = sprintf "[%2i, %2i]  %O  |  %O" (fst pos) (snd pos) (snd p1) (snd p2)
    

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

                let toNum status =
                    match status with
                    | Mate(p, t) -> (if p = board.Player then 1.0 else -1.0) * (10.0 - (float t)) * 1000000.0
                    | Check(p, t) -> (if p = board.Player then 1.0 else -1.0) * (10.0 - (float t)) * 10000.0
                    | InProgress(_, p) -> (p * 1000.0 |> int |> float) / 1000.0

                let allNodes = board.Candidates |> List.map (fun pos -> Node.New board pos)
                let notLoosingNodes = 
                    allNodes 
                    |> List.filter (fun node -> match node.StatusFor targetPlayer with | Mate(p, _) when p <> targetPlayer -> false | _ -> true)
                    |> List.sortBy (fun node -> node.TopStatus)
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
                                let opChecks = notLoosingNodes |> List.filter (isCheck adversary)
                                if opChecks.IsEmpty then myChecks else opChecks
                        | _ -> notLoosingNodes |> Seq.take 5 |> Seq.toList

                        

                match topMoves with
                | hd :: [] -> [(hd.Pos, 1.0)]
                | hd :: tl -> 
                    let expandedNodes = topMoves |> List.map (fun node -> node.Expand targetPlayer 2) |> List.sortBy (fun node -> node.Stats)
                    [(expandedNodes.Head.Pos, 2.0)]
                | [] -> [(allNodes.Head.Pos, -1.0)]
                //else

//                    let nextAs (board: Board) (player: Player) =
//                        seq { for cell in board.Cells double
//                                if cell.IsEmpty then
//                                    if Cell.K_Neighbours cell.Pos board.CellsMap 1 |> Seq.exists (fun c -> c.IsEmpty = false) then
//                                        let nextBoard = board.SetAs cell.Pos player
//                                        if nextBoard.IsSome then
//                                            let ai = HardAI(nextBoard.Value)
//                                            yield (cell.Pos, ai) } |> Seq.toList
//                    let next (board: Board) = nextAs board board.Player
//                    let addTestPos x y = 
//                        let ai = HardAI(board.SetAs (x, y) board.Player |> Option.get)
//                        ai.BoardStatus |> ignore
//                        ((x, y), ai)
//                    let nextBoards = next board
//                    //let nextBoards = [ addTestPos 7 12 ];//; addTestPos 13 13 ]
//
//
//
//                    let incTurns player turns = if player = targetPlayer then turns + 2 else turns + 1
//                    let analyzeGameTree (start: HardAI) =
//                        match start.BoardStatus with
//                        | Mate(_, 0) -> start.BoardStatus
//                        | _ ->
//                            let myPossibilities = nextAs start.Board targetPlayer |> List.sortBy snd |> List.map (fun (p, ai) -> (p, ai.BoardStatus))
//                            let opPossibilities = nextAs start.Board adversary |> List.sortBy snd |> List.map (fun (p, ai) -> (p, ai.BoardStatus))
//                            match (snd myPossibilities.Head, snd opPossibilities.Head) with
//                            | (Mate(p1, t1), Mate(p2, t2)) -> if incTurns p1 t1 < incTurns p2 t2 then Mate(p1, t1 + 2) else Mate(p2, t2 + 1)
//                            | (Mate(p1, t1), _) -> Mate(p1, t1 + 2)
//                            | (_, Mate(p, t)) -> Mate(p, t + 1)
//                            | (Check(p1, t1), Check(p2, t2)) -> if incTurns p1 t1 < incTurns p2 t2 then Check(p1, t1 + 2) else Check(p2, t2 + 1)
//                            | (Check(p1, t1), _) -> Check(p1, t1 + 2)
//                            | (_, Check(p, t)) -> Check(p, t + 1)
//                            | _ ->
//                                let analyzePossibilities (xs: (CellPos * BoardStatus) list) =
//                                    let acc = Array.create 3 0.0
//                                    for (_, bs) in xs do
//                                        match bs with
//                                        | Mate(_, _) -> acc.[0] <- acc.[0] + 1.0
//                                        | Check(_, _) -> acc.[1] <- acc.[1] + 1.0
//                                        | InProgress(_, p) -> if acc.[2] < p then acc.[2] <- p
//                                    acc
//                                let myAnalysis = analyzePossibilities myPossibilities
//                                InProgress(start.Board.Player, toNum start.BoardStatus * myAnalysis.[1])
//
//            //                let combinedBoads = 
//            //                    nextBoards 
//            //                    |> List.map (fun (pos, board) -> async { return (pos, analyzeGameTree board) }) 
//            //                    |> List.toArray 
//            //                    |> Async.Parallel 
//            //                    |> Async.RunSynchronously 
//            //                    |> Array.toList
//                    let combinedBoads = nextBoards |> List.map (fun (pos, board) -> (pos, analyzeGameTree board))
//                    let moves = combinedBoads |> List.map (fun (p, b) -> (p, toNum b)) |> List.sortBy snd |> List.rev
//                    if moves.IsEmpty then
//                        let center = (boardDimension / 2 + 1, boardDimension / 2 + 1)
//                        if board.CellsMap.[fst center].[snd center].IsEmpty then [ (center, 0.0) ]
//                        else if board.Cells |> Seq.isEmpty then []
//                        else [ ((board.Cells |> Seq.head).Pos, 0.0) ]
//                    else
//                        moves |> Seq.take 1 |> Seq.toList

        member x.Winner
            with get() =
                match x.BoardStatus with
                | Mate(p, 0) -> Some(p)
                | _ -> None