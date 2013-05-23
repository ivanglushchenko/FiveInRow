module MainApp

open System
open System.Windows
open System.Windows.Media
open System.Windows.Controls
open FSharpx
open FiveInRow.UI.Desktop

type MainWindow = XAML<"MainWindow.xaml">

//let b0 = Board.Create(9)
//
//let moves = [   (7, 8); (7, 7); 
//                (8, 7); (9, 8); 
//                (9, 7); (8, 6);
//                (9, 6); (8, 8);
//                (8, 9) ]
//

//
//let t1 = exec b0 moves
//t1.CalculateBestMove() |> ignore


let moves = [ (1, 1); (1, 2); 
              (2, 2); (2, 1); 
              (3, 1); (3, 2); ]

//let exec b (moves: (int * int) list) = moves |> List.fold (fun (acc: Board) m -> acc.Set m |> Option.get) b





let window = 
    let window = MainWindow()
    window.Root

type EventProc() = 
    let mutable _isCaptured = false
    let mutable _lastPos = new Point(0.0, 0.0)

    member x.Captured(p: Point) =
        _isCaptured <- true
        _lastPos <- p
        ()

    member this.Released(p: Point) =
        _isCaptured <- false
        ()

    member this.Moved(p: Point) =
        if _isCaptured then
            let offset = p - _lastPos
            ()
        ()



let eventProc = new EventProc()

let onMouseDown e =
    window.Background <- Brushes.Chocolate
    ()

let onManipulationStarting e = 
    window.Background <- Brushes.Red
    ()

let setupWindow (window: Window) =
    window.DataContext <- new MainWindowViewModel()
    window.RenderTransformOrigin <- Point(0.5, 0.5)
    window.MouseDown.Add (fun e -> eventProc.Captured(e.GetPosition(window)))
    window.MouseUp.Add (fun e -> eventProc.Released(e.GetPosition(window)))
    window.MouseMove.Add (fun e -> eventProc.Moved(e.GetPosition(window)))
    window.ManipulationStarting.Add onManipulationStarting
    window
    
[<STAThread>]
window |> setupWindow |> (new Application()).Run |> ignore