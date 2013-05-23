namespace FiveInRow.Foundation

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

    let neighboursOf (cell: Cell) = 
        seq { for i in fst cell.Pos - 1..fst cell.Pos + 1 do
                for j in snd cell.Pos - 1..snd cell.Pos + 1 do
                    if isValid (i, j) && (i <> fst cell.Pos || j <> snd cell.Pos) then
                        yield cells.[i].[j] }

    let replaceCell (cell: Cell) = 
        cells |> Map.map (fun row value -> 
            if row = fst cell.Pos then value |> Map.remove (snd cell.Pos) |> Map.add (snd cell.Pos) cell
            else value)

    let extendWith player (cell: Cell) = 
        let nearbyCells = neighboursOf cell |> Seq.filter (fun c -> c.IsOccupiedBy player) |> List.ofSeq
        let newRows = [ for c in nearbyCells -> Row.Create cell.Pos c.Pos ]

        let mergeRow (row: Row) (map: Map<RowKey, Row list>) =
            if map |> Map.containsKey row.Key then
                let rec loop (rows: Row list) = 
                    match rows with
                    | hd :: nk :: tl ->
                        if neighbours hd.To nk.From then Row.Merge hd nk :: tl |> loop
                        else hd :: loop (nk :: tl)
                    | _ -> rows
                let mergedRows = row :: map.[row.Key] |> List.sortBy (fun r -> r.StartPoint) |> loop
                map.Remove row.Key |> Map.add row.Key mergedRows
            else map.Add(row.Key, [ row ])

        let merge map = newRows |> List.fold (fun acc row -> mergeRow row acc) map

        let nextCells = Cell(cell.Pos, Occupied(player)) |> replaceCell
        let nextRows = rows |> Map.map (fun p value -> if p = player then merge value else value)

        for playerRows in nextRows do
            for rowGroup in playerRows.Value do
                for row in rowGroup.Value do
                    row.ResetRank nextCells

        Board(next player, nextCells, nextRows)

    let fitness = lazy (
        let calcFitness player = 
            let rowStats =
                seq { for rowGroup in rows.[player] do
                        for row in rowGroup.Value do
                            if row.Rank > 0 then yield (row.Length, row.Rank) }
                |> Seq.groupBy fst
                |> Seq.map (fun (k, v) -> (k, v |> Seq.map snd |> Seq.groupBy (fun t -> t) |> Seq.map (fun (k, v) -> (k, v |> Seq.length)) |> Map.ofSeq))
                |> Map.ofSeq
            let numOfRows length rank = 
                if rowStats.ContainsKey length && rowStats.[length].ContainsKey rank then rowStats.[length].[rank]
                else 0
            if rowStats.ContainsKey 5 then Win
            else if numOfRows 4 2 >= 1 then WinIn1Turn
            else if numOfRows 4 1 >= 2 then WinIn2Turns
            else if numOfRows 3 2 >= 2 then WinIn2Turns
            else if numOfRows 3 2 >= 1 && numOfRows 4 1 >= 1 then WinIn2Turns
            else Probability(rowStats |> Map.toSeq |> Seq.sumBy (fun (length, ranks) -> ranks |> Map.toSeq |> Seq.sumBy (fun (rank, count) -> (float count) * Math.Pow(2.0 * (float length), 1.0 + (float rank)))))
        Map.ofList [(Player1, calcFitness Player1); (Player2, calcFitness Player2)])

    let setAs (i, j) player =
        match cells.[i].[j].Value with
        | Occupied(_) -> None
        | Empty       -> Some(extendWith player cells.[i].[j])

    static member Create dim =
        boardDimension <- dim
        let cells = Map.ofList [ for i in 1..dim -> (i, Map.ofList [ for j in 1..dim -> (j, Cell((i, j), Empty)) ]) ]
        let rows = [ Player1; Player2] |> List.map (fun p -> (p, Map.empty<RowKey, Row list>)) |> Map.ofList
        Board(Player1, cells, rows)

    member x.Set (i, j) = setAs (i, j) currentPlayer

    member x.Player with get() = currentPlayer

    member x.Cells with get() = listOfCells.Value

    member x.Rows with get() = listOfRows.Value

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

    member x.Fitness with get() = fitness.Value

    member x.CalculateBestMove() =
        let possibleMoves = 
            seq { for cell in x.Cells do
                    if cell.IsEmpty then
                        if neighboursOf cell |> Seq.exists (fun c -> c.IsEmpty = false) then
                            yield cell }
        let opponent = next currentPlayer
        let possibleBoards = possibleMoves |> Seq.map (fun c -> (c, x.Set c.Pos, setAs c.Pos opponent))

        []