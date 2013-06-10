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
    let mutable showResults = false
    let boardView = BoardView.Create(GameSettings(19, Hard, Human))
    let boardView = BoardView.CreateFrom(GameSettings(19, Hard, Human), [(10, 10); (9, 9); (11, 10); (10, 8); (12, 10); (9, 10); (13, 10); (14, 10); (12, 11); (9, 7); (9, 8); (11, 7); (12, 6); (8, 7); (10, 9); (13, 12); (14, 9); (15, 8); (11, 12); (10, 13); (12, 12); (12, 13); (12, 8); (12, 9); (11, 11); (11, 9)])//; (11, 14); ])

    member x.Board with get() = boardView

    member x.Set index = 
        if boardView.IsCompleted = false then
            boardView.Set index

    member x.Offset
        with get() = offset
        and set(v) = 
            offset <- v
            x.OnPropertyChanged(<@ x.Offset @>)

    member x.OpponentHuman
        with get() = match boardView.Opponent with | Human -> true | _ -> false
        and set(v) =
            if v then
                boardView.Opponent <- Human
                x.OnPropertyChanged(<@ x.OpponentHuman @>)

    member x.OpponentPlayer1
        with get() = match boardView.Opponent with | AI(Player1) -> true | _ -> false
        and set(v) =
            if v then
                boardView.Opponent <- AI(Player1)
                x.OnPropertyChanged(<@ x.OpponentPlayer1 @>)

    member x.OpponentPlayer2
        with get() = match boardView.Opponent with | AI(Player2) -> true | _ -> false
        and set(v) =
            if v then
                boardView.Opponent <- AI(Player2)
                x.OnPropertyChanged(<@ x.OpponentPlayer2 @>)

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