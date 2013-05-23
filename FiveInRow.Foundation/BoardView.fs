namespace FiveInRow.Foundation

open GameDef

type BoardView(startingBoard: Board) =
    inherit ObservableObject()

    let mutable boards = [ startingBoard ]
    let mutable moves = []
    let cells = [| for r in 1..boardDimension -> [| for c in 1..boardDimension -> CellView(r, c) |] |]

    static member Create dim =
        let board = Board.Create dim
        boardDimension <- dim
        BoardView(board)

    member x.Cells = cells |> Array.collect (fun t -> t)

    member x.Rows with get() = boards.Head.Rows

    member x.Moves with get() = moves

    member x.FiveInRows with get() = boards.Head.Rows |> Seq.filter (fun r -> r.Length >= 5)

    member x.Set (i, j) =
        moves <- (i, j) :: moves
        x.OnPropertyChanged(<@ x.Moves @>)

        match boards.Head.Set (i, j) with
        | Some(board) -> 
            cells.[i - 1].[j - 1].Value <- Occupied(boards.Head.Player)
            cells.[i - 1].[j - 1].Fitness <- 0.0
            boards <- board :: boards
            for ((i, j), fitness) in board.BestMoves do
                cells.[i - 1].[j - 1].Fitness <- fitness
            x.OnPropertyChanged(<@ x.Rows @>)
            x.OnPropertyChanged(<@ x.FiveInRows @>)
        | None -> ()

    member x.MakeAIMove() =
        if boards.Head.BestMoves.IsEmpty = false then x.Set (fst boards.Head.BestMoves.Head)

    member x.Clear() =
        boards <- [ startingBoard ]
        for i in 1..boardDimension do
            for j in 1..boardDimension do
                cells.[i - 1].[j - 1].Value <- Empty
                cells.[i - 1].[j - 1].Fitness <- 0.0
        x.OnPropertyChanged(<@ x.Rows @>)
        x.OnPropertyChanged(<@ x.FiveInRows @>)

    member x.Winner 
        with get() = 
            match boards.Head.Fitness |> Map.toList |> List.filter (fun (p, f) -> f = Win) |> List.map fst with
            | hd :: tl -> Some(hd)
            | [] -> None