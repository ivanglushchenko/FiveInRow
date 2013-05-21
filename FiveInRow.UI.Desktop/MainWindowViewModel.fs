namespace FiveInRow.UI.Desktop

open System
open System.Collections.ObjectModel
open System.ComponentModel
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns
open FiveInRow.Foundation

type MainWindowViewModel() = 
    inherit ObservableObject()

    let v = new Cell((0, 0), List.empty, CellValue.O)

    let mutable _something = Array.empty<CellValue array>

    member x.Board 
        with get() = [| for i in 1..5 -> [| for _ in 1..5 -> CellValue.Unset |] |]
        and set(v : CellValue[][]) = 
            _something <- v
            x.OnPropertyChanged(<@ x.Board @>)