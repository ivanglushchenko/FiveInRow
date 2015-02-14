namespace FiveInRow.Core.UI

open System.Text
open FiveInRow.Core.GameDef
open FiveInRow.Core
open FiveInRow.Core.Threat
open FiveInRow.Core.AI
open FiveInRow.Core.Threats

type BoardInfo = { Board: Board.Board
                   Position: Position.Position
                   AI: AI
                   LastMove: Point option
                   LastPlayer: Player
                   Winner: Player option }

type BoardView(startingConf, startingOpponentType, ai: Player -> Board.Board -> Position.Position -> AI) =
    inherit ObservableObject()

    let (startingBoard, startingPosition, startingPlayer) = startingConf
    let mutable opponent: OpponentType = startingOpponentType
    let mutable boards = [ { Board = startingBoard
                             Position = startingPosition
                             AI = ai startingPlayer startingBoard startingPosition
                             LastMove = None
                             LastPlayer = next startingPlayer
                             Winner = None } ]
    let mutable isRunning = false
    let mutable showFitness = true
    let winnerChanged = new Event<Player option>()
    let cells = [| for r in 1..boardDimension -> [| for c in 1..boardDimension -> CellView(r, c) |] |]

    let clearCells() =
        for i in 1..boardDimension do
            for j in 1..boardDimension do
                cells.[i - 1].[j - 1].Value <- Empty
                cells.[i - 1].[j - 1].Fitness <- 0.0
                cells.[i - 1].[j - 1].IsLast <- false

    let clearLastMove() =
        match boards.Head.LastMove with
        | Some (r, c) -> cells.[r].[c].IsLast <- false
        | None -> ()
        match boards.Head.AI.NextMove with
        | Some (r, c) -> cells.[r].[c].Fitness <- 0.0
        | None -> ()

    let showMoves() =
        for (pos, p) in boards.Head.Board.Moves do
            cells.[fst pos].[snd pos].Value <- Occupied p

    let showPredictions() =
        if showFitness then
            match boards.Head.AI.NextMove with
            | Some (i, j) -> cells.[i].[j].Fitness <- 3.3
            | None -> ()

    let undo() =
        boards <- boards.Tail
        clearCells()
        for (pos, p) in boards.Head.Board.Moves do
            cells.[fst pos].[snd pos].Value <- Occupied p
        showPredictions()

    let collectThreats player threats =
        let isPlayer (p, _) = p = player
        threats 
            |> Seq.collect (snd >> Seq.filter isPlayer >> Seq.collect (snd >> SquareX.toSeq)) 
            |> Seq.choose id
            |> Seq.collect id
            |> Seq.sortBy (fun (_, t) -> t.Gain)

    static member Create (settings: GameSettings) = BoardView.CreateFrom (settings, [])

    static member CreateFrom (settings: GameSettings, moves) =
        boardDimension <- settings.BoardSize
        
        let (board, player) = Board.replay moves
        let (position, _) = Position.replay moves Position.empty
        let view = BoardView((board, position, player), settings.Opponent, AI.get2 settings.Difficulty)
        view.Opponent <- settings.Opponent
        view.Start()
        view

    member x.Cells = cells |> Array.collect (fun t -> t)

    member x.Rows with get() = Board.getRows boards.Head.Board

    member x.Moves 
        with get() = 
            let append (sb: StringBuilder) s =
                match s.LastMove with
                | Some (r, c) -> sb.AppendFormat("({0}, {1}); ", r, c)
                | _ -> sb.AppendFormat("?")
            boards |> List.rev |> List.fold append (new StringBuilder())

    member x.Set (i, j) =
        if x.IsCompleted = false && cells.[i].[j].Value = Empty then
            clearLastMove()

            let thisTurn = next boards.Head.LastPlayer

            cells.[i].[j].Value <- Occupied thisTurn
            cells.[i].[j].Fitness <- 0.0
            cells.[i].[j].IsLast <- true

            let board = Board.extend (i, j) thisTurn boards.Head.Board
            let position = Position.extend (i, j) thisTurn boards.Head.Position
            let ai = ai (next thisTurn) board position
            boards <- { Board = board
                        Position = position
                        AI = ai
                        LastMove = Some (i, j)
                        LastPlayer = thisTurn
                        Winner = if x.IsCompleted then x.Winner else Board.getWinner board } :: boards

            if x.IsCompleted = false then
                showPredictions()
                x.MakeMove()
                x.RaisePropertiesChanged()
            else
                ObservableObject.Post (fun () -> winnerChanged.Trigger(x.Winner))

            true
        else false

    member x.MakeMove() =
        if x.IsCompleted = false then
            match opponent with
                | AI(p) when p <> boards.Head.LastPlayer ->
                    match boards.Head.AI.NextMove with
                    | Some p -> x.Set p |> ignore
                    | None -> ()
                | _ -> ()
        
    member x.FastForward turns =
        for _ in 0..turns do
            if x.IsCompleted = false && boards.Head.AI.NextMove.IsSome then
                x.Set (boards.Head.AI.NextMove.Value) |> ignore

    member x.Clear() =
        boards <- [ boards |> List.rev |> List.head ]
        clearCells()
        x.RaisePropertiesChanged()

    member x.Start() =
        x.Clear()
        showMoves()
        x.MakeMove()
        showPredictions()

    member x.Winner with get() = boards.Head.Winner

    member x.IsCompleted with get() = x.Winner |> Option.isSome

    member x.FiveInRows with get() = (Board.getRows boards.Head.Board) |> Seq.filter (fun t -> t.Length = 5)

    member x.Undo() =
        if boards.Tail.IsEmpty = false then
            match opponent with
            | AI(Player1) when x.Moves.Length >= 3 ->
                undo()
                undo()
            | AI(Player2) -> 
                undo()
                undo()
            | Human -> 
                undo()
            | _ -> ()

        x.RaisePropertiesChanged()

    member x.NextTurn with get() = next boards.Head.LastPlayer

    member x.BestMove with get() = boards.Head.AI.NextMove

    member x.RaisePropertiesChanged() =
        x.OnPropertyChanged(<@ x.Moves @>)
        x.OnPropertyChanged(<@ x.Rows @>)
        x.OnPropertyChanged(<@ x.NextTurn @>)
        x.OnPropertyChanged(<@ x.Histograms @>)
        x.OnPropertyChanged(<@ x.ThreatsForPlayer1 @>)
        x.OnPropertyChanged(<@ x.ThreatsForPlayer2 @>)

    member x.Opponent
        with get() = opponent
        and set(v) =
            if v <> opponent then
                opponent <- v
                x.OnPropertyChanged(<@ x.Opponent @>)
                x.MakeMove()

    member x.IsRunning
        with get() = isRunning
        and set(v) =
            if v <> isRunning then
                isRunning <- v
                x.OnPropertyChanged(<@ x.IsRunning @>)

    member x.Histograms
        with get() = [| RowHistogram.print Player1 boards.Head.Board.Histogram; RowHistogram.print Player2 boards.Head.Board.Histogram |]

    [<CLIEvent>]
    member x.WinnerChanged = winnerChanged.Publish

    member x.ThreatsForPlayer1
        with get() = collectThreats Player1 boards.Head.Position.Threats

    member x.ThreatsForPlayer2
        with get() = collectThreats Player2 boards.Head.Position.Threats