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
    //let boardView = BoardView.CreateFrom(GameSettings(19, Hard, Human), [(7, 8); (6, 7); (6, 8); (5, 8); (5, 7); (7, 9); (5, 9); (7, 7); (4, 8); (6, 6); (3, 9); (4, 9);])
    //let boardView = BoardView.CreateFrom(GameSettings(19, Hard, Human), [(7, 8); (8, 8); (7, 9)])
    //let boardView = BoardView.CreateFrom(GameSettings(19, Hard, Human), [(5, 5); (5, 6); (6, 5); (6, 6);(5, 4); (6, 7); ])
    //let boardView = BoardView.CreateFrom(GameSettings(19, Hard, Human), [(5, 5); (5, 6); (6, 5); (6, 6);(5, 4); (6, 7); (6, 3); ])
    //let boardView = BoardView.CreateFrom(GameSettings(19, Hard, Human), [(8, 10); (9, 11); (8, 12); (8, 11); (7, 11); (10, 11); (6, 12); (11, 11); (12, 11); (9, 9); (9, 10); (12, 12);])// (10, 10); (11, 10);])// (6, 10); (7, 10);])// (6, 11); (13, 13);])// (5, 13); (4, 14); ])
    //let boardView = BoardView.CreateFrom(GameSettings(19, Hard, Human), [(6, 8); (5, 7); (5, 9); (7, 7); (4, 8); (3, 7); (4, 7); (4, 6); (4, 10); (4, 9); (5, 8); (3, 8); (3, 11); (2, 12); (3, 9); (7, 8); (5, 11); (6, 12);])// (3, 12); (3, 10); ])
    let boardView = BoardView.CreateFrom(GameSettings(19, Hard, Human), [(6, 9); (5, 8); (5, 10); (7, 8); (4, 9); (5, 9); (6, 11); (3, 8); (6, 10); (6, 8); (4, 8); (4, 10); (3, 11); (6, 12); (4, 11); (5, 11); (3, 9); (5, 7); (7, 12); (8, 13); (4, 13); (5, 12); (3, 12); (2, 13); (5, 14); (6, 15); (3, 14); (3, 13); (4, 14); (6, 14);])// (6, 13); (8, 11); ])

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