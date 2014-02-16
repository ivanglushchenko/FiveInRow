﻿namespace FiveInRow.Core.UI

open System.Text
open FiveInRow.Core.GameDef
open FiveInRow.Core
open FiveInRow.Core.AI

type BoardInfo = { Board: Board.Board
                   AI: AI
                   LastMove: Position option
                   LastPlayer: Player }

type BoardView(startingBoard: Board.Board, ai: Player -> Board.Board -> AI) =
    inherit ObservableObject()

    let mutable nextTurn = Player1
    let mutable opponent = Human
    let mutable boards = [ { Board = startingBoard; AI = ai Player1 startingBoard; LastMove = None; LastPlayer = Player2 } ]
    let mutable isRunning = false
    let mutable showFitness = true
    let winnerChanged = new Event<Player option>()
    let cells = [| for r in 1..boardDimension -> [| for c in 1..boardDimension -> CellView(r, c) |] |]

    let clearBoard() =
        for i in 1..boardDimension do
            for j in 1..boardDimension do
                cells.[i - 1].[j - 1].Value <- Empty
                cells.[i - 1].[j - 1].Fitness <- 0.0
                cells.[i - 1].[j - 1].IsLast <- false

    let undo() =
        boards <- boards.Tail
        clearBoard()
        for (pos, p) in boards.Head.Board.Moves do
            cells.[fst pos].[snd pos].Value <- Occupied p

    let clearLastMove() =
        match boards.Head.LastMove with
        | Some (r, c) -> cells.[r].[c].IsLast <- false
        | None -> ()

    let showPredictions() =
        if showFitness then
            for ((i, j), fitness) in boards.Head.AI.PossibleMoves do
                cells.[i].[j].Fitness <- fitness

    static member Create (settings: GameSettings) = BoardView.CreateFrom (settings, [])

    static member CreateFrom (settings: GameSettings, moves) =
        boardDimension <- settings.BoardSize
        
        let finalBoard = Board.replay moves
        let view =
            match settings.Difficulty with
            | Easy -> BoardView(finalBoard, AI.getEasy)
            | Medium -> BoardView(finalBoard, fun p b -> AI.empty)
            | Hard -> BoardView(finalBoard, fun p b -> AI.empty)
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

            cells.[i].[j].Value <- Occupied nextTurn
            cells.[i].[j].Fitness <- 0.0
            cells.[i].[j].IsLast <- true

            let board = Board.extend (i, j) nextTurn boards.Head.Board
            let ai = ai nextTurn board
            boards <- { Board = board; AI = ai; LastMove = Some (i, j); LastPlayer = nextTurn } :: boards

            if x.IsCompleted = false then
                showPredictions()
                nextTurn <- next nextTurn
                x.MakeMove()
                x.RaisePropertiesChanged()
//                x.IsRunning <- true
//                Async.Start
//                    (async {
//                        showPredictions()
//                        nextTurn <- next nextTurn
//                        x.MakeMove()
//                        x.RaisePropertiesChanged()
//                        x.IsRunning <- false })
            else
                ObservableObject.Post (fun () -> winnerChanged.Trigger(x.Winner))

    member x.MakeMove() =
        if x.IsCompleted = false then
            match opponent with
                | AI(p) when p = nextTurn ->
                    if boards.Head.AI.PossibleMoves.IsEmpty = false then x.Set (fst boards.Head.AI.PossibleMoves.Head)
                | _ -> ()
        
    member x.FastForward turns =
        for _ in 0..turns do
            if x.IsCompleted = false && boards.Head.AI.PossibleMoves.IsEmpty = false then
                x.Set (fst boards.Head.AI.PossibleMoves.Head)

    member x.Clear() =
        nextTurn <- Player1
        if boards.Length > 1 then
            boards <- [ { Board = startingBoard; AI = ai Player1 startingBoard; LastMove = None; LastPlayer = Player2 } ]
        clearBoard()
        x.RaisePropertiesChanged()

    member x.Start() =
        x.Clear()
        for (pos, p) in startingBoard.Moves do
            cells.[fst pos].[snd pos].Value <- Occupied p
        x.MakeMove()
        showPredictions()

    member x.Winner with get() = boards.Head.AI.Winner

    member x.IsCompleted with get() = x.Winner |> Option.isSome

    member x.FiveInRows with get() = (Board.getRows boards.Head.Board) |> Seq.filter (fun t -> t.Length = 5)

    member x.Undo() =
        if boards.Head.Board <> startingBoard then
            match opponent with
            | AI(Player1) when x.Moves.Length >= 3 ->
                undo()
                undo()
            | AI(Player2) -> 
                undo()
                undo()
            | Human -> undo()
            | _ -> ()

        x.RaisePropertiesChanged()

    member x.NextTurn with get() = nextTurn

    member x.BestMove with get() = fst boards.Head.AI.PossibleMoves.Head

    member x.RaisePropertiesChanged() =
        x.OnPropertyChanged(<@ x.Moves @>)
        x.OnPropertyChanged(<@ x.Rows @>)
        x.OnPropertyChanged(<@ x.NextTurn @>)

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
        with get() = [| RowHistogram.print Player1 boards.Head.Board.Histogram; RowHistogram.print Player1 boards.Head.Board.Histogram |]

    [<CLIEvent>]
    member x.WinnerChanged = winnerChanged.Publish