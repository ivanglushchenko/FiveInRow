namespace FiveInRow.UI.Desktop

open System
open System.Collections.Generic
open System.Linq
open System.Text
open System.Windows.Data
open System.Windows

open FiveInRow.Foundation.GameDef

type FitnessVisibilityConverter() =
    interface IValueConverter with
        member x.Convert(value, targetType, parameter, culture) =
            (if value :?> float > 0.0 then Visibility.Visible else Visibility.Collapsed) :> obj

        member x.ConvertBack(value, targetType, parameter, culture) = raise (Exception())