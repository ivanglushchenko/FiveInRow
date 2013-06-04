﻿namespace FiveInRow.Foundation

open GameDef
open System
open System.ComponentModel

type Board(currentPlayer: Player, cells: Map<int, Map<int, Cell>>, rows: Map<Player, Map<RowKey, Row list>>) =
    let listOfCells = lazy (
        seq { for row in cells do
                for cell in row.Value do
                    yield cell.Value })

    let listOfRows = lazy (
        seq { for playerRows in rows do
                for rowGroup in playerRows.Value do
                    for row in rowGroup.Value do
                        yield row })

    let replaceCell (cell: Cell) = 
        cells |> Map.map (fun row value -> 
            if row = fst cell.Pos then value |> Map.remove (snd cell.Pos) |> Map.add (snd cell.Pos) cell
            else value)

    let extendWith player (cell: Cell) = 
        let nearbyCells = Cell.Neighbours cell cells |> Seq.filter (fun c -> c.IsOccupiedBy player) |> List.ofSeq
        let newRows = [ for c in nearbyCells -> Row.Create cell.Pos c.Pos ]

        let mergeRow (row: Row) (map: Map<RowKey, Row list>) =
            if map |> Map.containsKey row.Key then
                let rec loop (rows: Row list) = 
                    match rows with
                    | hd :: nk :: tl ->
                        if neighbours hd.To nk.From then Row.Merge hd nk :: tl |> loop
                        else if hd.EndPoint > nk.EndPoint then hd :: tl |> loop
                        else hd :: loop (nk :: tl)
                    | _ -> rows
                let mergedRows = row :: map.[row.Key] |> List.sortBy (fun r -> r.StartPoint) |> loop
                map.Remove row.Key |> Map.add row.Key mergedRows
            else map.Add(row.Key, [ row ])

        let merge map = newRows |> List.fold (fun acc row -> mergeRow row acc) map

        let nextCells = Cell(cell.Pos, Occupied(player)) |> replaceCell
        let nextRows = rows |> Map.map (fun p value -> if p = player then merge value else value)

        let affectedRowKeys = Cell.Neighbours cell cells |> Seq.filter (fun c -> c.IsEmpty = false) |> Seq.map (fun c -> Row.Create cell.Pos c.Pos) |> Seq.map (fun r -> r.Key) |> Seq.toList

        for playerRows in nextRows do
            for rowKey in affectedRowKeys do
                if playerRows.Value.ContainsKey rowKey then
                    for row in playerRows.Value.[rowKey] do
                        row.ResetRank nextCells

        Board(next player, nextCells, nextRows)

    static member Create dim =
        boardDimension <- dim
        let cells = Map.ofList [ for i in 1..dim -> (i, Map.ofList [ for j in 1..dim -> (j, Cell((i, j), Empty)) ]) ]
        let rows = [ Player1; Player2] |> List.map (fun p -> (p, Map.empty<RowKey, Row list>)) |> Map.ofList
        Board(Player1, cells, rows)

    member x.Set (i, j) = x.SetAs (i, j) currentPlayer

    member x.SetAs (i, j) player =
        match cells.[i].[j].Value with
        | Occupied(_) -> None
        | Empty       -> Some(extendWith player cells.[i].[j])

    member x.Player with get() = currentPlayer

    member x.Cells with get() = listOfCells.Value

    member x.CellsMap with get() = cells

    member x.Rows with get() = listOfRows.Value

    member x.RowsMap with get() = rows

    override x.ToString() =
        let sb = new System.Text.StringBuilder()
        for row in cells do
            for cell in row.Value do
                (match cell.Value.Value with
                | Empty -> sb.Append "."
                | Occupied(Player1) -> sb.Append "x"
                | Occupied(Player2) -> sb.Append "o") |> ignore
                sb.AppendLine() |> ignore
        sb.ToString()