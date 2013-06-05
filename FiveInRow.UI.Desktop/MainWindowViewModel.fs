namespace FiveInRow.UI.Desktop

open System
open System.Windows
open System.Collections.ObjectModel
open System.ComponentModel
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns

open FiveInRow.Foundation.GameDef
open FiveInRow.Foundation

type MainWindowViewModel() = 
    inherit ObservableObject()

    let mutable offset = Vector(0.0, 0.0)
    let mutable useAI = false
    let mutable showResults = false
    let boardView = BoardView.Create(GameSettings(51, Hard, if useAI then AI(Player2) else Human))
    //let boardView = BoardView.CreateFrom(GameSettings(19, Medium), [ (9, 9); (8, 8); (7, 8); (8, 9); (8, 10); (7, 11); (7, 9); (6, 8); (6, 10); (7, 10); (9, 8); (9, 10); (6, 11); (6, 12); (7, 12); (5, 10); (9, 11); (10, 12); (7, 13); (8, 12); (8, 7); (10, 9); (9, 6); (10, 5); (9, 7); (9, 5); (7, 7); (6, 7); (10, 7); (11, 7); (7, 6); (7, 5); (8, 5); (7, 4); (11, 8); (12, 9); (12, 8); (6, 6); (6, 5); (5, 4)])//; (10, 8); (11, 9); (13, 8); (14, 8) ])

    member x.Board with get() = boardView

    member x.Set index = 
        if boardView.IsCompleted = false then
            boardView.Set index

    member x.Offset
        with get() = offset
        and set(v) = 
            offset <- v
            x.OnPropertyChanged(<@ x.Offset @>)

    member x.UseAI
        with get() = useAI
        and set(v) = 
            useAI <- v

    member x.ShowResults
        with get() = showResults
        and set(v) =
            if v <> showResults then
                showResults <- v
                if v then
                    let winningRow = boardView.Rows |> Seq.filter (fun r -> r.Length >= 5)
                    ()

    member x.Start() = boardView.Start()

    member x.MakeMove() = boardView.MakeMove boardView.NextTurn