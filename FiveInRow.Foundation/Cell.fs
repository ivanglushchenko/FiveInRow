namespace FiveInRow.Foundation

open System.ComponentModel

type CellValue =
    | Unset
    | AI_UnderConsideration
    | X
    | O

type CellIndex = { x: int; y: int }

type Cell(_index: (int * int), pointsTo: (int * int) list, value: CellValue) =
    inherit ObservableObject()

    let mutable _value = value
    let mutable _probability = 0.0

    member x.Index with get() = _index

    member x.X with get() = fst _index

    member x.Y with get() = snd _index

    member x.Neighbors with get() = pointsTo

    member x.Set(cv: CellValue) =
        x.Value <- cv

    member x.SetProbability(i) =
        x.Probability <- i

    member x.IsEmpty() =
        match x.Value with | X | O -> false | _ -> true
 
    override x.ToString() =
        "V: " + (fst _index).ToString() + ", " + (snd _index).ToString()

    member x.Value
        with get() = _value
        and  set(v) =
            _value <- v
            x.OnPropertyChanged(<@ x.Value @>)

    member x.Probability
        with get() = _probability
        and  set(v) =
            _probability <- v
            x.OnPropertyChanged(<@ x.Probability @>)

