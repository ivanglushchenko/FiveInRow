﻿namespace FiveInRow.Core

open FiveInRow.GameMechanics.GameDef

type CellValue = Empty | Occupied of Player

type CellView(row: int, col: int) =
    inherit ObservableObject()

    let mutable value = Empty
    let mutable fitness = 0.0
    let mutable isLast = false

    member x.Row with get() = row

    member x.Col with get() = col

    member x.Value
        with get() = value
        and set(v) =
            if v <> value then
                value <- v
                x.OnPropertyChanged(<@ x.Value @>)

    member x.Fitness
        with get() = fitness
        and set(v) =
            if v <> fitness then
                fitness <- v
                x.OnPropertyChanged(<@ x.Fitness @>)

    member x.IsLast
        with get() = isLast
        and set(v) =
            if v <> isLast then
                isLast <- v
                x.OnPropertyChanged(<@ x.IsLast @>)