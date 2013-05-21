module MainApp

open System
open System.Windows
open System.Windows.Controls
open FSharpx
open FiveInRow.UI.Desktop

type MainWindow = XAML<"MainWindow.xaml">

let loadWindow() =
   let window = MainWindow()
   window.Root.DataContext <- new MainWindowViewModel()
   window.Root

[<STAThread>]
(new Application()).Run(loadWindow()) |> ignore