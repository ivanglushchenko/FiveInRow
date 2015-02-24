// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.

open FiveInRow.Core
open FiveInRow.Core.GameDef
open FiveInRow.Core.UI
open FiveInRow.Core.Threat

let run caption f =
    let sw = System.Diagnostics.Stopwatch()
    sw.Start()
    let res = f()
    sw.Stop()
    diag (sprintf "%s: %O" caption sw.Elapsed)
    res

let threatToString (kind, data) =
    sprintf "%O - %O" kind data

let rec treeToStrings tree indent =
    seq {
        match tree with
        | Some nodes ->
            let prefix = System.String(' ', indent * 4)
            for node in nodes do
                yield prefix + (threatToString node.Threat)
                yield! treeToStrings node.Dependencies (indent + 1)
        | None -> () }

[<EntryPoint>]
let main argv =
    boardDimension <- 19
    let moves = [ (6, 5); (6, 4); (6, 6); (7, 4); (6, 7); (8, 4); (7, 6); (9, 4); (10, 4); (10, 3); (10, 5); (11, 5); (10, 6); (9, 6); (9, 5); (9, 8); (8, 5); (7, 5); (8, 6); (8, 8); (8, 7); (7, 7); (5, 4); (4, 3); ]

    let analyzeTree list = 
        match list with
        | Some xs -> Threats.analyzeTree xs
        | None -> None

//    let res1 = run "board" (fun () ->
//        let (board, player) = Board.replay moves
//        Threats.buildThreatsTreeForBoard Player1 board 10 ) |> analyzeTree

    let res2 = run "position" (fun () ->
        let (position, player) = Position.replay moves Position.empty
        AI.analyzeThreatSpace Player1 10 position )
        //Threats.buildThreatsTreeForPosition Player1 board 10 )
             

    //for s in threatsStrings do diag s
    //diag (sprintf "%O vs %O" res1 res2)
    diag (sprintf "%O" res2)
    0 // return an integer exit code
