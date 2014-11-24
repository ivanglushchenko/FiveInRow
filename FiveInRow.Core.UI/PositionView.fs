namespace FiveInRow.Core.UI

open FiveInRow.Core.GameDef
open FiveInRow.Core.Threats

type PositionView(boardView: BoardView) =
    inherit ObservableObject()

    //let position = Position.empty boardDimension

    let cells = [| for r in 1..boardDimension -> [| for c in 1..boardDimension -> CellView(r, c) |] |]
    let mutable currentPlayer = Player1
    let mutable lastMove: (int * int) option = None

    let clearCells() =
        for i in 1..boardDimension do
            for j in 1..boardDimension do
                cells.[i - 1].[j - 1].Value <- Empty
                cells.[i - 1].[j - 1].Fitness <- 0.0
                cells.[i - 1].[j - 1].IsLast <- false

    static member Create(boardView) =
        let pv = PositionView(boardView)
        //boardView.
        pv

    member x.Cells = cells |> Array.collect (fun t -> t)

    member x.Set (r, c) =
        cells.[r].[c].Value <- Occupied currentPlayer
        cells.[r].[c].Fitness <- 0.0
        cells.[r].[c].IsLast <- true

        currentPlayer <- next currentPlayer

        lastMove <- Some(r, c)

    member x.Start() =
        clearCells()
        //x.Clear()
        //showMoves()
        //x.MakeMove()
        //showPredictions()

    member x.RaisePropertiesChanged() =
        //x.OnPropertyChanged(<@ x.Moves @>)
        //x.OnPropertyChanged(<@ x.Rows @>)
        //x.OnPropertyChanged(<@ x.NextTurn @>)
        //x.OnPropertyChanged(<@ x.Histograms @>)
        ()