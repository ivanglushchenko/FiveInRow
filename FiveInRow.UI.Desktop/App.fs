module MainApp

open System
open System.Windows
open System.Windows.Media
open System.Windows.Controls
open FSharpx
open FiveInRow.UI.Desktop

type MainWindow = XAML<"MainWindow.xaml">

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