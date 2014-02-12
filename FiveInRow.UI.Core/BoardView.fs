namespace FiveInRow.Core

open FiveInRow.GameMechanics
open FiveInRow.GameMechanics.GameDef
open FiveInRow.GameMechanics.Board
open FiveInRow.GameMechanics.AI
open System.Text

type BoardInfo = { Board: Board
                   AI: AI
                   LastMove: Position option
                   LastPlayer: Player }

type BoardView(startingBoard: Board, ai: Board -> AI) =
    inherit ObservableObject()

    let mutable nextTurn = Player1
    let mutable opponent = Human
    let mutable boards = [ { Board = startingBoard; AI = ai startingBoard; LastMove = None; LastPlayer = Player2 } ]
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
        for c in boards.Head.Board.Moves do
            cells.[fst c.Key].[snd c.Key].Value <- Occupied c.Value

    static member Create (settings: GameSettings) = BoardView.CreateFrom (settings, [])

    static member CreateFrom (settings: GameSettings, moves) =
        boardDimension <- settings.BoardSize

        let rec exec moves p b =
            match moves with
            | hd :: tl -> Board.extend (hd, p) b |> exec tl (next p)
            | [] -> b
        
        let finalBoard = exec moves Player1 Board.empty
        let view =
            match settings.Difficulty with
            | Easy -> BoardView(finalBoard, fun b -> AI.empty)
            | Medium -> BoardView(finalBoard, fun b -> AI.empty)
            | Hard -> BoardView(finalBoard, fun b -> AI.empty)
        view.Opponent <- settings.Opponent
        view.Start()
        view

    member x.Cells = cells |> Array.collect (fun t -> t)

    member x.Rows with get() = boards.Head.Board.Rows

    member x.Moves 
        with get() = 
            let append (sb: StringBuilder) s = sb.AppendFormat("({0}, {1}); ", (fst (Option.get s.LastMove)), (snd (Option.get s.LastMove)))
            boards |> List.rev |> List.fold append (new StringBuilder())

    member x.Set (i, j) =
        if x.IsCompleted = false then
            let board = Board.extend ((i, j), nextTurn) boards.Head.Board

            match boards.Head.LastMove with
            | Some (r, c) -> cells.[r].[c].IsLast <- false
            | None -> ()

            cells.[i - 1].[j - 1].Value <- Occupied nextTurn
            cells.[i - 1].[j - 1].Fitness <- 0.0
            cells.[i - 1].[j - 1].IsLast <- true

            let ai = ai board
            boards <- { Board = board; AI = ai; LastMove = Some (i, j); LastPlayer = nextTurn } :: boards
            if x.IsCompleted = false then
                x.IsRunning <- true
                Async.Start
                    (async {
                        if showFitness then
                            for ((i, j), fitness) in ai.PossibleMoves do
                                cells.[i - 1].[j - 1].Fitness <- fitness
                        x.MakeMove nextTurn
                        x.RaisePropertiesChanged()
                        x.IsRunning <- false })
            else
                ObservableObject.Post (fun () -> winnerChanged.Trigger(x.Winner))


    member x.MakeMove player =
        if x.IsCompleted = false then
            match opponent with
                | AI(p) when p = player ->
                    if boards.Head.AI.PossibleMoves.IsEmpty = false then x.Set (fst boards.Head.AI.PossibleMoves.Head)
                | _ -> ()
        

    member x.Clear() =
        boards <- [ { Board = startingBoard; AI = ai startingBoard; LastMove = None; LastPlayer = Player2 } ]
        clearBoard()
        x.RaisePropertiesChanged()

    member x.Start() =
        x.Clear()
        for c in startingBoard.Moves do
            cells.[fst c.Key].[snd c.Key].Value <- Occupied c.Value
        x.MakeMove Player1

    member x.Winner with get() = boards.Head.AI.Winner

    member x.IsCompleted with get() = x.Winner |> Option.isSome

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
                match opponent with
                | AI(p) when boards.Head.LastPlayer = p -> x.MakeMove p
                | _ -> ()

    member x.IsRunning
        with get() = isRunning
        and set(v) =
            if v <> isRunning then
                isRunning <- v
                x.OnPropertyChanged(<@ x.IsRunning @>)

    [<CLIEvent>]
    member x.WinnerChanged = winnerChanged.Publish