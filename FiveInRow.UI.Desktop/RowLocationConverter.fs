namespace FiveInRow.UI.Desktop

open System
open System.Collections.Generic
open System.Linq
open System.Text
open System.Windows.Data

type RowLocationConverter() =
    interface IMultiValueConverter with
        member x.Convert(values, targetType, parameter, culture) =
            let offset = if parameter = null then 0.5 else 0.0
            (((float(values.[0] :?> int) + offset) * 50.0) + (values.[1] :?> float)) :> obj

        member x.ConvertBack(value, targetTypes, parameter, culture) = raise (Exception())