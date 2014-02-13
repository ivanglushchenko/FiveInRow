namespace FiveInRow.UI.Desktop

open System
open System.Windows
open System.Collections.ObjectModel
open System.ComponentModel
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns

open FiveInRow.GameMechanics.GameDef
//open FiveInRow.Foundation.Simulation
open FiveInRow.GameMechanics
open FiveInRow.Core

type MainWindowViewModel() = 
    inherit ObservableObject()

    let mutable offset = Vector(0.0, 0.0)
    let mutable showResults = false
    let boardView = BoardView.Create(GameSettings(19, Easy, Human))
    //let boardView = BoardView.CreateFrom(GameSettings(19, Hard, Human), [(3, 10);  (3, 11);  (4, 11); (2, 8);])// (3, 9); ])
    //let boardView = BoardView.CreateFrom(GameSettings(19, Hard, Human), [(7, 8); (6, 7); (6, 8); (5, 8); (5, 7); (7, 9); (5, 9); (7, 7); (4, 8); (6, 6); (3, 9); (4, 9);])
    //let boardView = BoardView.CreateFrom(GameSettings(19, Hard, Human), [(7, 8); (8, 8); (7, 9)])
    //let boardView = BoardView.CreateFrom(GameSettings(19, Hard, Human), [(5, 5); (5, 6); (6, 5); (6, 6);(5, 4); (6, 7); ])
    //let boardView = BoardView.CreateFrom(GameSettings(19, Hard, Human), [(5, 5); (5, 6); (6, 5); (6, 6);(5, 4); (6, 7); (6, 3); ])
    //let boardView = BoardView.CreateFrom(GameSettings(19, Hard, Human), [(8, 10); (9, 11); (8, 12); (8, 11); (7, 11); (10, 11); (6, 12); (11, 11); (12, 11); (9, 9); (9, 10); (12, 12);])// (10, 10); (11, 10);])// (6, 10); (7, 10);])// (6, 11); (13, 13);])// (5, 13); (4, 14); ])
    //let boardView = BoardView.CreateFrom(GameSettings(19, Hard, Human), [(6, 8); (5, 7); (5, 9); (7, 7); (4, 8); (3, 7); (4, 7); (4, 6); (4, 10); (4, 9); (5, 8); (3, 8); (3, 11); (2, 12); (3, 9); (7, 8); (5, 11); (6, 12);])// (3, 12); (3, 10); ])
    //let boardView = BoardView.CreateFrom(GameSettings(19, Hard, Human), [(6, 9); (5, 8); (5, 10); (7, 8); (4, 9); (5, 9); (6, 11); (3, 8); (6, 10); (6, 8); (4, 8); (4, 10); (3, 11); (6, 12); (4, 11); (5, 11); (3, 9); (5, 7); (7, 12); (8, 13); (4, 13); (5, 12); (3, 12); (2, 13); (5, 14); (6, 15); (3, 14); (3, 13); (4, 14); (6, 14);])// (6, 13); (8, 11); ])
    //let boardView = BoardView.CreateFrom(GameSettings(19, Hard, Human), [(6, 9); (5, 8); (5, 10); (4, 11); (4, 9); (5, 9); (6, 11); (7, 12); (7, 10); (6, 10); (5, 12); (8, 9); (9, 12); (4, 13); ])//(9, 11); (9, 13); ])
    //let boardView = BoardView.CreateFrom(GameSettings(19, Hard, Human), [(8, 8); (7, 7); (7, 9); (6, 10); (6, 8); (7, 8); (5, 7); (4, 6); (8, 10); (9, 11); (8, 9); (8, 11); (8, 7); (8, 6); (6, 9); (9, 9); (5, 9); (4, 9); (5, 8); (5, 6); (5, 10); (5, 11); (9, 7); (7, 11); (6, 11); (6, 6); (7, 6); (3, 6); (2, 6); (10, 11); (11, 11); (9, 10); (9, 8); (10, 9); (11, 8); (7, 10); (10, 8); (12, 8); (10, 6); (11, 5); (12, 10); (9, 12);])// (9, 13); (10, 12);])// (11, 9); (13, 11);])// (11, 10); (1, 5); (11, 12); ])
    //let boardView = BoardView.CreateFrom(GameSettings(19, Hard, Human), [(9, 9); (8, 8); (8, 10); (7, 11); (7, 9); (8, 9); (9, 11); (10, 12); (9, 10); (9, 12); (8, 12); (10, 10); (6, 8); (5, 7); (8, 13); (10, 11); (8, 11); (8, 14); (10, 13); (10, 9); (10, 8); (9, 13); (11, 11); (7, 15); (6, 16); (11, 7); (7, 12); (6, 13); (6, 11); (5, 10); (9, 14); (10, 15); (8, 15); (11, 12); (7, 16); (6, 17); (5, 16); (4, 16); (8, 16); (9, 16); (6, 10); (6, 9); (6, 15); (8, 17); (5, 14); (7, 18); ])
    //let boardView = BoardView.CreateFrom(GameSettings(19, Hard, Human), [(8, 8); (7, 7); (7, 9); (6, 10); (6, 8); (7, 8); (5, 7); (4, 6); (4, 8); (8, 10); (3, 7); (7, 10); (5, 10); (9, 10);])// (10, 10); (8, 9); (6, 7); (6, 6); (5, 8); (5, 9); (4, 7); (2, 7); (3, 6); (2, 5); (3, 8); (1, 4); ])
    //let boardView = BoardView.CreateFrom(GameSettings(19, Hard, Human), [(9, 9); (8, 8); (8, 9); (10, 9); (7, 8); (6, 7); (9, 8); (7, 10); (10, 7); (11, 6); (11, 8); (9, 7); (9, 10); (10, 6); (7, 9); (8, 7); (10, 8); (8, 10); (7, 7); (9, 6); (8, 6); (11, 5); (12, 4); (12, 6); (13, 6); (11, 4); (11, 3); (10, 5); (12, 3);])// (13, 4); (5, 9); (6, 9); (6, 8); (4, 10); ])

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
                    //let winningRow = boardView.Rows |> Seq.filter (fun r -> r.Length >= 5)
                    ()

    member x.Start() = boardView.Start()

    member x.MakeMove() = boardView.MakeMove boardView.NextTurn

    member x.Simulate() =
//        let diffs = [ Medium; Easy; Hard ]
//        let pairs =
//            seq { for d1 in diffs do
//                    for d2 in diffs do
//                        if d1 <> d2 then
//                            yield (d1, d2, function | Player1 -> d1 | _ -> d2)
//                            yield (d2, d1, function | Player1 -> d2 | _ -> d1) }
//            |> Seq.toList
//        let test (d1, d2, f) =
//            let battle = Battle(d1, d2)
//            match battle.Play() with
//            | Some(p) -> (d1, d2, Some(f p))
//            | None -> (d1, d2, None)
//        let results = pairs |> List.map test
//
//        for (d1, d2, res) in results do
//            printfn "%O x %O -> %O" d1 d2 res
        ()