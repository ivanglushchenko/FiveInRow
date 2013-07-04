namespace FiveInRow.Foundation

open System
open System.Collections.ObjectModel
open System.ComponentModel
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns

type ObservableObject() = 
    static let mutable syncContext = System.Threading.SynchronizationContext.Current
    static let mutable post = 
        fun (callback: unit -> unit) -> 
            Async.RunSynchronously
                (async {
                    // Switch to the UI thread and update the UI. 
                    do! Async.SwitchToContext(syncContext)
                    callback()
                    // Switch back to the thread pool. 
                    do! Async.SwitchToThreadPool()
                })

    let propertyChanged = new Event<_, _>()

    interface INotifyPropertyChanged with
        [<CLIEvent>]
        member x.PropertyChanged = propertyChanged.Publish
 
    abstract member OnPropertyChanged: string -> unit

    default x.OnPropertyChanged(propertyName : string) = post (fun () -> propertyChanged.Trigger(x, new PropertyChangedEventArgs(propertyName)))

    static member Post 
        with get() = post
        and set(v) = post <- v

 
    member x.OnPropertyChanged(expr : Expr) =
        match expr with
        | PropertyGet(_, b, _) -> x.OnPropertyChanged(b.Name)
        | _ -> raise (new Exception())
