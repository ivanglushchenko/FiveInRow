namespace FiveInRow.UI.Desktop

open System
open System.Collections.Generic
open System.Linq
open System.Text
open System.Windows.Data
open System.Windows.Media

type ThreatOptionConverter() =
    interface IValueConverter with
        member x.Convert(value, targetType, parameter, culture) =
            failwith "ss"

        member x.ConvertBack(value, targetTypes, parameter, culture) =
            if value = null then None :> obj
            else (Some (value :?> FiveInRow.Core.Threat.Threat)) :> obj
