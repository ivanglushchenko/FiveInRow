module FiveInRow.Core.RowHistogram

open System.Text
open GameDef

type RowHistogram = RowHistogram of int array

let inline create() = Array.create 24 0 |> RowHistogram

let inline inc player length rank (RowHistogram histogram) =
    if isTracingEnabled then
        System.Diagnostics.Debug.WriteLine(sprintf "inc %O %O %O" player length rank)

    if length < 6 then
        let i = ((length - 2) + 4 * rank) + (if player = Player1 then 0 else 12)
        histogram.[i] <- histogram.[i] + 1

let inline dec player length rank (RowHistogram histogram) =
    if isTracingEnabled then
        System.Diagnostics.Debug.WriteLine(sprintf "dec %O %O %O" player length rank)

    if length < 6 then
        let i = ((length - 2) + 4 * rank) + (if player = Player1 then 0 else 12)
        histogram.[i] <- histogram.[i] - 1

let inline hasLength player length (RowHistogram histogram) =
    let i = length - 2 + (if player = Player1 then 0 else 12)
    histogram.[i] > 0 || histogram.[i + 4] > 0 || histogram.[i + 8] > 0

let inline getCount player length rank (RowHistogram histogram) =
    histogram.[((length - 2) + 4 * rank) + (if player = Player1 then 0 else 12)]

let score player scorer (RowHistogram histogram) =
    let offset = if player = Player1 then 0 else 12
    (float histogram.[offset + 0]) * scorer 2 0 +
    (float histogram.[offset + 1]) * scorer 3 0 +
    (float histogram.[offset + 2]) * scorer 4 0 +
    (float histogram.[offset + 3]) * scorer 5 0 +
    (float histogram.[offset + 4]) * scorer 2 1 +
    (float histogram.[offset + 5]) * scorer 3 1 +
    (float histogram.[offset + 6]) * scorer 4 1 +
    (float histogram.[offset + 7]) * scorer 5 1 +
    (float histogram.[offset + 8]) * scorer 2 2 +
    (float histogram.[offset + 9]) * scorer 3 2 +
    (float histogram.[offset + 10]) * scorer 4 2 +
    (float histogram.[offset + 11]) * scorer 5 2

let inline clone (RowHistogram histogram) =
    Array.copy histogram |> RowHistogram

let print player (RowHistogram histogram) =
    let sb = StringBuilder()
    let columnHeaders = System.String.Join(" ", [| for r in 2..5 -> "L" + r.ToString() |])
    sb.AppendFormat("{0}   {1}\r\n", player, columnHeaders) |> ignore
    for r in 0..2 do
        let line = System.String.Join("  ", [| for l in 2..5 -> (getCount player l r (RowHistogram histogram)).ToString() |])
        sb.AppendFormat("r{0}|  {1}\r\n", r, line) |> ignore
    sb.ToString()