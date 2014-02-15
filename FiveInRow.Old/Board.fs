namespace FiveInRow.Foundation

open GameDef
open System
open System.ComponentModel
open FiveInRow.Core.GameDef
open FiveInRow.Core.UI

type Board(lastPos: Position, currentPlayer: Player, cMap: Map<int, Map<int, Cell>>, rMap: Map<Player, Map<RowKey, Row list>>, hash: Set<int * int>) =
    let listOfCells = lazy (
        seq { for row in cMap do
                for cell in row.Value do
                    yield cell.Value })

    let listOfRows = lazy (
        seq { for playerRows in rMap do
                for rowGroup in playerRows.Value do
                    for row in rowGroup.Value do
                        yield row })

    let replaceCell (cell: Cell) = 
        cMap |> Map.map (fun row value -> 
            if row = fst cell.Pos then value |> Map.remove (snd cell.Pos) |> Map.add (snd cell.Pos) cell
            else value)

    let extendWith player (cell: Cell) = 
        let nearbyCells = Cell.Neighbours cell.Pos cMap |> Seq.filter (fun c -> c.IsOccupiedBy player) |> List.ofSeq
        let newRows = [ for c in nearbyCells -> Row.Create cMap cell.Pos c.Pos ]
        
        let replaceRow (rows: Map<Player, Map<RowKey, Row list>>) (player: Player, oldRow: Row, newRow: Row) =
            rows |> Map.map 
                (fun p pRow -> 
                    if p = player then 
                        pRow |> Map.map 
                            (fun rKey rList ->
                                if rKey = oldRow.Key then newRow :: (rList |> List.filter (fun r -> r <> oldRow))
                                else rList)
                    else pRow)

        let mergeRow (row: Row) (map: Map<RowKey, Row list>) =
            if map |> Map.containsKey row.Key then
                let rec loop (rows: Row list) = 
                    match rows with
                    | hd :: nk :: tl ->
                        if neighbours hd.To nk.From then Row.Merge cMap hd nk :: tl |> loop
                        else if hd.EndPoint > nk.EndPoint then hd :: tl |> loop
                        else hd :: loop (nk :: tl)
                    | _ -> rows
                let mergedRows = row :: map.[row.Key] |> List.sortBy (fun r -> r.StartPoint) |> loop
                map.Remove row.Key |> Map.add row.Key mergedRows
            else map.Add(row.Key, [ row ])

        let rec mergeRows (rows: Row list) (rowsMap: Map<Player, Map<RowKey, Row list>>) =
            match rows with
            | hd :: tl ->
                let newPlayerRowsMap = mergeRow hd rowsMap.[player]
                let newRowsMap = rowsMap |> Map.map (fun p value -> if p = player then newPlayerRowsMap else value)
                mergeRows tl newRowsMap
            | [] -> rowsMap

        let nextCells = Cell(cell.Pos, Occupied(player)) |> replaceCell
        let nextRows = mergeRows newRows rMap

        let affectedRowKeys = Cell.Neighbours cell.Pos cMap |> Seq.filter (fun c -> c.IsEmpty = false) |> Seq.map (fun c -> Row.Create nextCells cell.Pos c.Pos) |> Seq.map (fun r -> r.Key) |> Seq.toList

        let affectedRows = 
            seq { for playerRows in nextRows do
                    for rowKey in affectedRowKeys do
                        if playerRows.Value.ContainsKey rowKey then
                            for row in playerRows.Value.[rowKey] do
                                match row.UpdateRank nextCells with
                                | Some(r) -> yield (playerRows.Key, row, r)
                                | _ -> () }
            |> Seq.toList
        let updatedNextRows = affectedRows |> List.fold (fun acc row -> replaceRow acc row) nextRows
        let nextHash = hash.Add cell.Pos

        Board(cell.Pos, next player, nextCells, updatedNextRows, nextHash)

    let candidates = lazy ( 
        seq { for cell in listOfCells.Value do
                if cell.IsEmpty then
                    if Cell.K_Neighbours cell.Pos cMap 1 |> Seq.exists (fun c -> c.IsEmpty = false) then
                        yield cell.Pos } |> List.ofSeq)

    let groupId = lazy (
        let rowSignatures=
            seq { for playerRows in rMap do
                    for rowGroup in playerRows.Value do
                        for row in rowGroup.Value do
                            yield sprintf "%O:%O:%i:%i" playerRows.Key rowGroup.Key row.StartPoint row.EndPoint }
            |> Seq.sort
            |> Seq.toArray
        String.Join(" ", rowSignatures))

    static member Create dim =
        boardDimension <- dim
        let cells = Map.ofList [ for i in 1..dim -> (i - 1, Map.ofList [ for j in 1..dim -> (j - 1, Cell((i - 1, j - 1), Empty)) ]) ]
        let rows = [ Player1; Player2] |> List.map (fun p -> (p, Map.empty<RowKey, Row list>)) |> Map.ofList
        Board((-1, -1), Player1, cells, rows, Set.empty)

    member x.Set (i, j) = x.SetAs (i, j) currentPlayer

    member x.SetAs (i, j) player =
        match cMap.[i].[j].Value with
        | Occupied(_) -> None
        | Empty       -> 
            let t = Some(extendWith player cMap.[i].[j])
            //System.Diagnostics.Debug.WriteLine(t.Value.Print())
            t

    member x.Player with get() = currentPlayer

    member x.SwitchPlayer() = Board(lastPos, next currentPlayer, cMap, rMap, hash)

    member x.Cells with get() = listOfCells.Value

    member x.CellsMap with get() = cMap

    member x.Rows with get() = listOfRows.Value

    member x.RowsMap with get() = rMap

    member x.Candidates with get() = candidates.Value

    member x.LastPos with get() = lastPos

    member x.Print() =
        let sb = new System.Text.StringBuilder()
        sb.AppendLine(x.ToString()) |> ignore
        for row in cMap do
            for cell in row.Value do
                (match cell.Value.Value with
                | Empty -> sb.Append "."
                | Occupied(Player1) -> sb.Append "x"
                | Occupied(Player2) -> sb.Append "o") |> ignore
            sb.AppendLine() |> ignore
        sb.ToString()

    override x.ToString() = sprintf "curr: %O, last %O -> [%i, %i]" currentPlayer (next currentPlayer) (fst lastPos) (snd lastPos)

    member x.GroupId with get() = groupId.Value

    member x.Hash with get() = hash