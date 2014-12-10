// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.

open FiveInRow.Core
open FiveInRow.Core.GameDef
open FiveInRow.Core.UI

let run caption f =
    let sw = System.Diagnostics.Stopwatch()
    sw.Start()
    f()
    sw.Stop()
    diag (sprintf "%s: %O" caption sw.Elapsed)


[<EntryPoint>]
let main argv = 
    let sw = System.Diagnostics.Stopwatch()
    sw.Start()

    let moves = [ (6, 5); (6, 4); (6, 6); (7, 4); (6, 7); (8, 4); (7, 6); (9, 4); (10, 4); (10, 3); (10, 5); (11, 5); (10, 6); (9, 6); (9, 5); (9, 8); (8, 5); (7, 5); (8, 6); (8, 8); (8, 7); (7, 7); ]
    let boardView = BoardView.CreateFrom(GameSettings(19, Impossible, Human), moves)
    let threats = boardView.Threats
    
    sw.Stop()
    System.Diagnostics.Debug.WriteLine(sw.Elapsed.ToString())

    printfn "%A" argv
    0 // return an integer exit code
