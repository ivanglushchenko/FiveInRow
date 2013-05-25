module MainApp

open System
open System.Windows
open System.Windows.Media
open System.Windows.Controls
open FSharpx
open FiveInRow.UI.Desktop
open FiveInRow.Foundation

type MainWindow = XAML<"MainWindow.xaml">

//let t = System.Environment.TickCount
//let b = FiveInRow.Foundation.BoardView.Create(35)
//for i in 1..35 do
//    for j in 1..35 do
//        b.Set (i, j)
//let dt = System.Environment.TickCount - t
//System.Diagnostics.Debug.Write(dt)
//printfn ""

let createWindow =
    let window = MainWindow()
    let viewModel = new MainWindowViewModel()
    window.BtnRestart.Click.Add (fun _ -> viewModel.Restart())
    window.BtnMakeMove.Click.Add (fun _ -> viewModel.MakeMove())
    window.Root.DataContext <- viewModel
    window.Root.RenderTransformOrigin <- Point(0.5, 0.5)
    window.Root
    
[<STAThread>]
createWindow |> (new Application()).Run |> ignore