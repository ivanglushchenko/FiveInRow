namespace FiveInRow.UI.Desktop

open System
open System.Collections.Generic
open System.Linq
open System.Text
open System.Windows.Data

open FiveInRow.Core.GameDef
open FiveInRow.Core.UI

type CellValueToStrConverter() =
    interface IValueConverter with
        member x.Convert(value, targetType, parameter, culture) =
            match value :?> SquareStatus with
            | Empty -> "Empty" :> obj
            | Occupied(Player1) -> "Player1" :> obj
            | Occupied(Player2) -> "Player2" :> obj

        member x.ConvertBack(value, targetType, parameter, culture) = raise (Exception())