namespace FiveInRow.UI.Desktop

open System
open System.Collections.Generic
open System.Linq
open System.Text
open System.Windows.Data
open System.Windows

type BoolToVisibilityConverter() =
    interface IValueConverter with
        member x.Convert(value, targetType, parameter, culture) =
            (if (value :?> bool) = true then Visibility.Visible else Visibility.Collapsed) :> obj

        member x.ConvertBack(value, targetTypes, parameter, culture) = raise (Exception())