namespace FiveInRow.Foundation

open GameDef
open GameAI
open System.Text

type BoardInfo = { board: Board; ai: AI }

type BoardView(startingBoard: Board, ai: Board -> AI) =
    inherit ObservableObject()

    let mutable opponent = Human
    let mutable boards = [ { board = startingBoard; ai = ai startingBoard } ]
    let mutable moves = []
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
        moves <- moves.Tail
        clearBoard()
        for c in boards.Head.board.Cells |> Seq.filter (fun c -> c.IsEmpty = false) do
            cells.[fst c.Pos - 1].[snd c.Pos - 1].Value <- c.Value

    static member Create (settings: GameSettings) = BoardView.CreateFrom (settings, [])

    static member CreateFrom (settings: GameSettings, moves) =
        let exec (moves: (int * int) list) b = moves |> List.fold (fun (acc: Board) m -> acc.Set m |> Option.get) b
        let board = Board.Create settings.BoardSize
        boardDimension <- settings.BoardSize
        let finalBoard = board |> exec moves
        let view =
            match settings.Difficulty with
            | Easy -> BoardView(finalBoard, fun b -> EasyAI(b) :> AI)
            | Medium -> BoardView(finalBoard, fun b -> MediumAI(b) :> AI)
            | Hard -> BoardView(finalBoard, fun b -> HardAI(b) :> AI)
        view.Opponent <- settings.Opponent
        view.Start()
        view

    member x.Cells = cells |> Array.collect (fun t -> t)

    member x.Rows with get() = boards.Head.board.Rows

    member x.Moves 
        with get() = 
            let append (sb: StringBuilder) s = sb.AppendFormat("({0}, {1}); ", (fst s), (snd s))
            moves |> List.rev |> List.fold append (new StringBuilder())

    member x.FiveInRows with get() = boards.Head.board.Rows |> Seq.filter (fun r -> r.Length >= 5)

    member x.Set (i, j) =
        if x.IsCompleted = false then
            match boards.Head.board.Set (i, j) with
            | Some(board) -> 
                if moves.IsEmpty = false then
                    cells.[fst moves.Head - 1].[snd moves.Head - 1].IsLast <- false
                moves <- (i, j) :: moves
                cells.[i - 1].[j - 1].Value <- Occupied(boards.Head.board.Player)
                cells.[i - 1].[j - 1].Fitness <- 0.0
                cells.[i - 1].[j - 1].IsLast <- true
                //if showFitness then
                //    for ((i, j), fitness) in boards.Head.ai.Moves do
                //        cells.[i - 1].[j - 1].Fitness <- 0.0
                let ai = ai board
                boards <- { board = board; ai = ai } :: boards
                if x.IsCompleted = false then
                    x.IsRunning <- true
                    Async.Start
                        (async {
                            if showFitness then
                                for ((i, j), fitness) in ai.Moves do
                                    cells.[i - 1].[j - 1].Fitness <- fitness
                            board.Player |> x.MakeMove
                            x.RaisePropertiesChanged()
                            x.IsRunning <- false })
                else
                    ObservableObject.Post (fun () -> winnerChanged.Trigger(x.Winner))
            | None -> ()

    member x.MakeMove player =
        if x.IsCompleted = false then
            match opponent with
                | AI(p) when p = player ->
                    if boards.Head.ai.Moves.IsEmpty = false then x.Set (fst boards.Head.ai.Moves.Head)
                | _ -> ()
        

    member x.Clear() =
        boards <- [ { board = startingBoard; ai = ai startingBoard } ]
        moves <- []
        clearBoard()
        x.RaisePropertiesChanged()

    member x.Start() =
        x.Clear()
        for c in startingBoard.Cells |> Seq.filter (fun c -> c.IsEmpty = false) do
            cells.[fst c.Pos - 1].[snd c.Pos - 1].Value <- c.Value
        x.MakeMove Player1

    member x.Winner with get() = boards.Head.ai.Winner

    member x.IsCompleted with get() = x.Winner |> Option.isSome

    member x.Undo() =
        if boards.Head.board <> startingBoard then
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

    member x.NextTurn with get() = boards.Head.board.Player

    member x.BestMove with get() = fst boards.Head.ai.Moves.Head

    member x.RaisePropertiesChanged() =
        x.OnPropertyChanged(<@ x.Moves @>)
        x.OnPropertyChanged(<@ x.Rows @>)
        x.OnPropertyChanged(<@ x.FiveInRows @>)
        x.OnPropertyChanged(<@ x.NextTurn @>)

    member x.Opponent
        with get() = opponent
        and set(v) =
            if v <> opponent then
                opponent <- v
                x.OnPropertyChanged(<@ x.Opponent @>)
                match opponent with
                | AI(p) when boards.Head.board.Player = p -> x.MakeMove p
                | _ -> ()

    member x.IsRunning
        with get() = isRunning
        and set(v) =
            if v <> isRunning then
                isRunning <- v
                x.OnPropertyChanged(<@ x.IsRunning @>)

    [<CLIEvent>]
    member x.WinnerChanged = winnerChanged.Publish