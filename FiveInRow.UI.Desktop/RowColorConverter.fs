namespace FiveInRow.UI.Desktop

open System
open System.Collections.Generic
open System.Linq
open System.Text
open System.Windows.Data
open System.Windows.Media

type RowColorConverter() =
    interface IValueConverter with
        member x.Convert(value, targetType, parameter, culture) =
            match value :?> int with
            | 2 -> Brushes.Green :> obj
            | 1 -> Brushes.Orange :> obj
            | 0 -> Brushes.Red :> obj
            | _ -> failwith "bad rank"

        member x.ConvertBack(value, targetTypes, parameter, culture) = raise (Exception())
