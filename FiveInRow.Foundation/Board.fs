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

        let affectedRowKeys = neighboursOf cell |> Seq.filter (fun c -> c.IsEmpty = false) |> Seq.map (fun c -> Row.Create cell.Pos c.Pos) |> Seq.map (fun r -> r.Key) |> Seq.toList

        for playerRows in nextRows do
            for rowKey in affectedRowKeys do
                if playerRows.Value.ContainsKey rowKey then
                    for row in playerRows.Value.[rowKey] do
                        row.ResetRank nextCells

        Board(next player, nextCells, nextRows)

    let setAs (i, j) player =
        match cells.[i].[j].Value with
        | Occupied(_) -> None
        | Empty       -> Some(extendWith player cells.[i].[j])

    let fitness = lazy (
        let calcFitness player = 
            let rowStats =
                let array = Array.init 6 (fun i -> Array.create 3 0)
                for rowGroup in rows.[player] do
                    for row in rowGroup.Value do
                        if row.Length <= 5 then
                            let trimmedLength = min row.Length 5
                            array.[trimmedLength].[row.Rank] <- array.[trimmedLength].[row.Rank] + 1
                array
            let numOfRows length rank = rowStats.[length].[rank]
            if rowStats.[5] |> Array.exists (fun n -> n > 0) then Win
            else if numOfRows 4 2 >= 1 then WinIn1Turn
            else if numOfRows 4 1 >= 2 then WinIn2Turns
            else if numOfRows 3 2 >= 2 then WinIn2Turns
            else if numOfRows 3 2 >= 1 && numOfRows 4 1 >= 1 then WinIn2Turns
            else 
                let sumRanks length ranks = ranks |> Array.mapi (fun rank count -> (float count) * Math.Pow(2.0 * (float length), 1.0 + (float rank))) |> Array.sum
                Probability(rowStats |> Array.mapi sumRanks |> Array.sum)
        Map.ofList [(Player1, calcFitness Player1); (Player2, calcFitness Player2)])

    let bestMoves = lazy (
        let possibleMoves = 
            seq { for cell in listOfCells.Value do
                    if cell.IsEmpty then
                        if neighboursOf cell |> Seq.exists (fun c -> c.IsEmpty = false) then
                            yield cell }
        let opponent = next currentPlayer
        let getFitness pos player = (setAs pos player |> Option.get).Fitness |> Map.find player
        let possibleBoards = possibleMoves |> Seq.map (fun c -> (c, getFitness c.Pos currentPlayer, getFitness c.Pos opponent))  |> Seq.toList

        let myWins = possibleBoards |> List.filter (fun (v, f1, f2) -> match f1 with | Probability _ -> false | _ -> true) |> List.map (fun (v, f1, f2) -> f1) |> Set.ofList
        let opWins = possibleBoards |> List.filter (fun (v, f1, f2) -> match f2 with | Probability _ -> false | _ -> true) |> List.map (fun (v, f1, f2) -> f2) |> Set.ofList

        let noMyWin = myWins.Contains Win = false
        let noOpWin = opWins.Contains Win = false
        let noWin = noMyWin && noOpWin
        let noMyWinIn1Turn = myWins.Contains WinIn1Turn = false
        let noOpWinIn1Turn = opWins.Contains WinIn1Turn = false
        let noWinIn1Turn = noMyWinIn1Turn && noOpWinIn1Turn
        let noMyWinIn2Turns = myWins.Contains WinIn2Turns = false
        let noOpWinIn2Turns = opWins.Contains WinIn2Turns = false
        let noWinIn2Turns = noMyWinIn2Turns && noOpWinIn2Turns

        let combine f1 f2 =
            match (f1, f2) with
            | (Win, _)                                                                     -> Double.PositiveInfinity
            | (_, Win)                       when noMyWin                                  -> Double.PositiveInfinity
            | (WinIn1Turn, _)                when noWin                                    -> Double.PositiveInfinity
            | (_, WinIn1Turn)                when noWin && noMyWinIn1Turn                  -> Double.PositiveInfinity
            | (WinIn2Turns, _)               when noWin && noWinIn1Turn                    -> Double.PositiveInfinity
            | (_, WinIn2Turns)               when noWin && noWinIn1Turn && noMyWinIn2Turns -> Double.PositiveInfinity
            | _ -> (match f1 with | Probability p -> p | _ -> 0.0) + (match f2 with | Probability p -> p | _ -> 0.0)

        [ for (cell, f1, f2) in possibleBoards -> (cell.Pos, combine f1 f2) ] |> List.sortBy (fun (k, v) -> -v))

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

    member x.BestMoves with get() = bestMoves.Value