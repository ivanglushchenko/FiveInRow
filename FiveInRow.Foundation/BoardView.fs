namespace FiveInRow.Foundation

open GameDef

type CellView(row: int, col: int) =
    inherit ObservableObject()

    let mutable value = Empty
    let mutable fitness = 0.0

    member x.Row with get() = row

    member x.Col with get() = col

    member x.Value
        with get() = value
        and set(v) =
            value <- v
            x.OnPropertyChanged(<@ x.Value @>)

    member x.Fitness
        with get() = fitness
        and set(v) =
            fitness <- v
            x.OnPropertyChanged(<@ x.Fitness @>)


type BoardView(board: Board) =
    inherit ObservableObject()

    let mutable useAI = true
    let mutable boards = [ board ]
    let mutable moves = []
    let cells = [| for r in 1..boardDimension -> [| for c in 1..boardDimension -> CellView(r, c) |] |]

    static member Create dim =
        let board = Board.Create dim
        boardDimension <- dim
        BoardView(board)

    member x.UseAI
        with get() = useAI
        and set(v) = useAI <- v

    member x.Cells = cells |> Array.collect (fun t -> t)

    member x.Rows with get() = boards.Head.Rows

    member x.Moves with get() = moves

    member x.Set (i, j) =
        moves <- (i, j) :: moves
        x.OnPropertyChanged(<@ x.Moves @>)

        match boards.Head.Set (i, j) with
        | Some(board) -> 
            cells.[i - 1].[j - 1].Value <- Occupied(boards.Head.Player)
            boards <- board :: boards
            x.OnPropertyChanged(<@ x.Rows @>)
        | None -> ()

