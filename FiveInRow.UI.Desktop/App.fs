module MainApp

open System
open System.Windows
open System.Windows.Media
open System.Windows.Controls
open FSharpx

open FiveInRow.UI.Desktop

type MainWindow = XAML<"MainWindow.xaml">

let createWindow =
    let window = MainWindow()
    let viewModel = new MainWindowViewModel()
    window.btnRestart.Click.Add (fun _ -> viewModel.Start())
    window.btnPositionRestart.Click.Add (fun _ -> viewModel.Start())
    window.btnMakeMove.Click.Add (fun _ -> viewModel.MakeMove())
    window.btnSimulate.Click.Add (fun _ -> viewModel.Simulate())
    window.btnUndo.Click.Add (fun _ -> viewModel.Board.Undo())
    window.Root.DataContext <- viewModel
    window.Root.RenderTransformOrigin <- Point(0.5, 0.5)
    window.Root
    
[<STAThread>]
createWindow |> (new Application()).Run |> ignore