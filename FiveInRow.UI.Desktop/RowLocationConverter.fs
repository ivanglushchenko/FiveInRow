namespace FiveInRow.UI.Desktop

open System
open System.Collections.Generic
open System.Linq
open System.Text
open System.Windows.Data

type RowLocationConverter() =
    interface IMultiValueConverter with
        member x.Convert(values, targetType, parameter, culture) =
            (((float(values.[0] :?> int) - 0.5) * 50.0) + (values.[1] :?> float)) :> obj

        member x.ConvertBack(value, targetTypes, parameter, culture) = raise (Exception())