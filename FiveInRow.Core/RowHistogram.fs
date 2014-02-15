module FiveInRow.Core.RowHistogram

open GameDef

type RowHistogram = RowHistogram of int array

let inline create() = Array.create 24 0 |> RowHistogram

let inline inc player length rank (RowHistogram histogram) =
    if length < 6 then
        let i = ((length - 2) + 4 * rank) * (if player = Player1 then 1 else 2)
        histogram.[i] <- histogram.[i] + 1

let inline dec player length rank (RowHistogram histogram) =
    if length < 6 then
        let i = ((length - 2) + 4 * rank) * (if player = Player1 then 1 else 2)
        histogram.[i] <- histogram.[i] - 1

let inline hasLength player length (RowHistogram histogram) =
    let i = length - 2
    let offset = if player = Player1 then 0 else 12
    histogram.[offset + i] > 0 || histogram.[offset + i + 4] > 0 || histogram.[offset + i + 8] > 0

let inline getCount player length rank (RowHistogram histogram) =
    histogram.[((length - 2) + 4 * rank) * (if player = Player1 then 1 else 2)]

let inline score player scorer (RowHistogram histogram) =
    let offset = if player = Player1 then 0 else 12
    (float histogram.[offset + 0]) * scorer 2 0 +
    (float histogram.[offset + 1]) * scorer 3 0 +
    (float histogram.[offset + 2]) * scorer 4 0 +
    (float histogram.[offset + 3]) * scorer 5 0 +
    (float histogram.[offset + 4]) * scorer 2 1 +
    (float histogram.[offset + 5]) * scorer 3 1 +
    (float histogram.[offset + 6]) * scorer 4 1 +
    (float histogram.[offset + 7]) * scorer 5 1 +
    (float histogram.[offset + 8]) * scorer 2 1 +
    (float histogram.[offset + 9]) * scorer 3 1 +
    (float histogram.[offset + 10]) * scorer 4 1 +
    (float histogram.[offset + 11]) * scorer 5 1

let inline clone (RowHistogram histogram) =
    Array.copy histogram |> RowHistogram