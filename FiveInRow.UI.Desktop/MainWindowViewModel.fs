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

    let mutable offset = Vector(0.0, 0.0)
    let boardView = BoardView.Create(51)

    member x.Board with get() = boardView

    member x.Set index = boardView.Set index

    member x.Offset
        with get() = offset
        and set(v) = 
            offset <- v
            x.OnPropertyChanged(<@ x.Offset @>)

    member x.UseAI
        with get() = boardView.UseAI
        and set(v) = boardView.UseAI <- v