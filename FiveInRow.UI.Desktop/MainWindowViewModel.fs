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
    let mutable useAI = false
    let mutable showResults = false
    let boardView = BoardView.Create(51)

    member x.Board with get() = boardView

    member x.Set index = 
        if x.IsCompleted = false then
            boardView.Set index
            if x.IsCompleted = false && useAI then boardView.MakeAIMove()

    member x.Offset
        with get() = offset
        and set(v) = 
            offset <- v
            x.OnPropertyChanged(<@ x.Offset @>)

    member x.UseAI
        with get() = useAI
        and set(v) = useAI <- v

    member x.Restart() = boardView.Clear()

    member x.IsCompleted with get() = boardView.Winner |> Option.isSome

    member x.ShowResults
        with get() = showResults
        and set(v) =
            if v <> showResults then
                showResults <- v
                if v then
                    let winningRow = boardView.Rows |> Seq.filter (fun r -> r.Length >= 5)
                    ()

    member x.MakeMove() = boardView.MakeAIMove()