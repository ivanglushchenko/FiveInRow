namespace FiveInRow.UI.Desktop

open System
open System.Windows
open System.Collections.ObjectModel
open System.ComponentModel
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns
open FiveInRow.Foundation

type MainWindowViewModel() = 
    inherit ObservableObject()

    let mutable _offset = Vector(0.0, 0.0)
    let _board = Board.CreateNew(51, 51)

    member x.Board with get() = _board

    member x.Graph with get() = _board.Graph

    member x.Set(index: (int * int)) =
        _board.Set index |> ignore
        x.OnPropertyChanged(<@ x.Graph @>)
        ()

    member x.Offset
        with get() = _offset
        and set(v) = 
            _offset <- v
            x.OnPropertyChanged(<@ x.Offset @>)

    member x.Player1
        with get() = _board.ControlsXPlayer
        and set(v) = _board.ControlsXPlayer <- v

    member x.Player2
        with get() = _board.ControlsOPlayer
        and set(v) = _board.ControlsOPlayer <- v