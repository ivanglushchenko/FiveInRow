namespace FiveInRow.UI.Desktop

open System
open System.Collections.Generic
open System.Linq
open System.Text
open System.Windows.Data
open System.Windows

open FiveInRow.Foundation.GameDef

type PlayerToStrConverter() =
    interface IValueConverter with
        member x.Convert(value, targetType, parameter, culture) =
            match value :?> Player with
            | Player1 -> "X" :> obj
            | Player2 -> "O" :> obj


        member x.ConvertBack(value, targetType, parameter, culture) = raise (Exception())