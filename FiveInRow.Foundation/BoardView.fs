namespace FiveInRow.Foundation

open GameDef

type BoardView(startingBoard: Board) =
    inherit ObservableObject()

    let mutable boards = [ startingBoard ]
    let mutable moves = []
    let cells = [| for r in 1..boardDimension -> [| for c in 1..boardDimension -> CellView(r, c) |] |]

    do
        for c in startingBoard.Cells |> Seq.filter (fun c -> c.IsEmpty = false) do
            cells.[fst c.Pos - 1].[snd c.Pos - 1].Value <- c.Value

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
        for c in boards.Head.Cells |> Seq.filter (fun c -> c.IsEmpty = false) do
            cells.[fst c.Pos - 1].[snd c.Pos - 1].Value <- c.Value

    static member Create (settings: GameSettings) =
        let board = Board.Create settings.BoardSize
        boardDimension <- settings.BoardSize
        difficulty <- settings.Difficulty
        opponent <- settings.Opponent
        let view = BoardView(board)
        view.Start()
        view

    static member CreateFrom (settings: GameSettings, moves) =
        let exec (moves: (int * int) list) b = moves |> List.fold (fun (acc: Board) m -> acc.Set m |> Option.get) b
        let board = Board.Create settings.BoardSize
        boardDimension <- settings.BoardSize
        difficulty <- settings.Difficulty
        opponent <- settings.Opponent
        BoardView(board |> exec moves)

    member x.Cells = cells |> Array.collect (fun t -> t)

    member x.Rows with get() = boards.Head.Rows

    member x.Moves with get() = moves

    member x.FiveInRows with get() = boards.Head.Rows |> Seq.filter (fun r -> r.Length >= 5)

    member x.Set (i, j) =
        if x.IsCompleted = false then
            match boards.Head.Set (i, j) with
            | Some(board) -> 
                if moves.IsEmpty = false then 
                    cells.[fst moves.Head - 1].[snd moves.Head - 1].IsLast <- false
                moves <- (i, j) :: moves
                cells.[i - 1].[j - 1].Value <- Occupied(boards.Head.Player)
                cells.[i - 1].[j - 1].Fitness <- 0.0
                cells.[i - 1].[j - 1].IsLast <- true
                boards <- board :: boards
                for ((i, j), fitness) in board.BestMoves do
                    cells.[i - 1].[j - 1].Fitness <- fitness
                x.RaisePropertiesChanged()

                if x.IsCompleted = false then
                    match opponent with
                    | AI(p) when p = board.Player -> x.MakeAIMove()
                    | _ -> ()

            | None -> ()

    member x.MakeAIMove() =
        if boards.Head.BestMoves.IsEmpty = false then x.Set (fst boards.Head.BestMoves.Head)

    member x.Clear() =
        boards <- [ startingBoard ]
        moves <- []
        clearBoard()
        x.RaisePropertiesChanged()

    member x.Start() =
        x.Clear()
        match opponent with
        | AI(Player1) -> x.MakeAIMove()
        | _ -> ()

    member x.Winner 
        with get() = 
            match boards.Head.Fitness |> Map.toList |> List.filter (fun (p, f) -> f = Win) |> List.map fst with
            | hd :: tl -> Some(hd)
            | [] -> None

    member x.IsCompleted with get() = x.Winner |> Option.isSome

    member x.Undo() =
        if boards.Head <> startingBoard then
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

    member x.NextTurn with get() = boards.Head.Player

    member x.BestMove with get() = fst boards.Head.BestMoves.Head

    member x.RaisePropertiesChanged() =
        x.OnPropertyChanged(<@ x.Moves @>)
        x.OnPropertyChanged(<@ x.Rows @>)
        x.OnPropertyChanged(<@ x.FiveInRows @>)
        x.OnPropertyChanged(<@ x.NextTurn @>)