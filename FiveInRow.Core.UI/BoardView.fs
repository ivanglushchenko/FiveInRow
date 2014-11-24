namespace FiveInRow.Core.UI

open System.Text
open FiveInRow.Core.GameDef
open FiveInRow.Core
open FiveInRow.Core.AI

type BoardInfo = { Board: Board.Board
                   AI: AI
                   LastMove: Point option
                   LastPlayer: Player
                   Winner: Player option }

type BoardView(startingConfiguration, startingOpponentType, ai: Player -> Board.Board -> AI) =
    inherit ObservableObject()

    let mutable opponent: OpponentType = startingOpponentType
    let mutable boards = [ { Board = fst startingConfiguration
                             AI = ai (snd startingConfiguration) (fst startingConfiguration)
                             LastMove = None
                             LastPlayer = (snd >> next) startingConfiguration
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
        for (r, c), _ in boards.Head.AI.PossibleMoves do
            cells.[r].[c].Fitness <- 0.0

    let showMoves() =
        for (pos, p) in boards.Head.Board.Moves do
            cells.[fst pos].[snd pos].Value <- Occupied p

    let showPredictions() =
        if showFitness then
            for ((i, j), fitness) in boards.Head.AI.PossibleMoves do
                cells.[i].[j].Fitness <- fitness

    let undo() =
        boards <- boards.Tail
        clearCells()
        for (pos, p) in boards.Head.Board.Moves do
            cells.[fst pos].[snd pos].Value <- Occupied p
        showPredictions()

    static member Create (settings: GameSettings) = BoardView.CreateFrom (settings, [])

    static member CreateFrom (settings: GameSettings, moves) =
        boardDimension <- settings.BoardSize
        
        let finalBoard = Board.replay moves
        let view =
            match settings.Difficulty with
            | Easy -> BoardView(finalBoard, settings.Opponent, AI.getEasy)
            | Medium -> BoardView(finalBoard, settings.Opponent, AI.getMedium)
            | Hard -> BoardView(finalBoard, settings.Opponent, AI.getHard)
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
            let ai = ai (next thisTurn) board
            boards <- { Board = board; AI = ai; LastMove = Some (i, j); LastPlayer = thisTurn; Winner = if x.IsCompleted then x.Winner else Board.getWinner board } :: boards

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
                    if boards.Head.AI.PossibleMoves.IsEmpty = false then x.Set (fst boards.Head.AI.PossibleMoves.Head) |> ignore
                | _ -> ()
        
    member x.FastForward turns =
        for _ in 0..turns do
            if x.IsCompleted = false && boards.Head.AI.PossibleMoves.IsEmpty = false then
                x.Set (fst boards.Head.AI.PossibleMoves.Head) |> ignore

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

    member x.BestMove with get() = fst boards.Head.AI.PossibleMoves.Head

    member x.RaisePropertiesChanged() =
        x.OnPropertyChanged(<@ x.Moves @>)
        x.OnPropertyChanged(<@ x.Rows @>)
        x.OnPropertyChanged(<@ x.NextTurn @>)
        x.OnPropertyChanged(<@ x.Histograms @>)
        x.OnPropertyChanged(<@ x.Threats @>)

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

    member x.Threats
        with get() = 
            match boards with 
            | hd :: _ -> 
                let threats = Threats.identifyThreatsUnconstrained (next hd.LastPlayer) hd.Board |> Seq.toArray
                threats
            | _ -> [| |]