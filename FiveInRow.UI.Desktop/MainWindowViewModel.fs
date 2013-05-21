namespace FiveInRow.UI.Desktop

open System
open System.Collections.ObjectModel
open System.ComponentModel
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns

type Cell = Empty | X | O

type MainWindowViewModel() = 

    let propertyChanged = new Event<_, _>()

    interface INotifyPropertyChanged with
        [<CLIEvent>]
        member x.PropertyChanged = propertyChanged.Publish
 
    abstract member OnPropertyChanged: string -> unit

    default x.OnPropertyChanged(propertyName : string) =
        propertyChanged.Trigger(x, new PropertyChangedEventArgs(propertyName))
 
    member x.OnPropertyChanged(expr : Expr) =
        match expr with
        | PropertyGet(_, b, _) -> x.OnPropertyChanged(b.Name)
        | _ -> raise (new Exception())

    member x.Board with get() = [| for i in 1..5 -> [| for _ in 1..5 -> Empty |] |]