module FiveInRow.GameMechanics.Board

open GameDef
open Row
open RowX

type Board = { Moves: Map<Position, Player>
               Rows: Map<Position, RowX> }

let empty = { Moves = Map.empty
              Rows = Map.empty } 

let getRow pos dir rows =
    if Map.containsKey pos rows then RowX.get dir rows.[pos]
    else None

let setRow pos dir row rows =
    if Map.containsKey pos rows then
        let extRj = rows.[pos]
        let r = rows.Remove pos
        let newRj = RowX.update dir (Some row) extRj
        r.Add (pos, newRj)
    else
        rows.Add (pos, RowX.update dir (Some row) RowX.empty)

let getRowLength pos dir board =
    (getRow pos dir board.Rows |> Option.get).Length

let getRowRank pos dir board =
    (getRow pos dir board.Rows |> Option.get).Rank

let nullifyRow pos dir rows =
    let extRj = Map.find pos rows
    let newRj = RowX.update dir None extRj
    if RowX.isEmpty newRj then rows.Remove pos
    else (rows.Remove pos).Add (pos, newRj)

let extend ((row, col), player) board =
    if board.Moves.ContainsKey (row, col) then failwith "Cell is occupied already"

    let newMoves = board.Moves.Add ((row, col), player)

    let possibleRows =
        seq { yield (row - 1, col - 1), (row + 1, col + 1), SE
              yield (row - 1, col), (row + 1, col), S
              yield (row - 1, col + 1), (row + 1, col - 1), SW
              yield (row, col - 1), (row, col + 1), E }

    let generateNewRows (p1, p2, dir) =
        match (board.Moves.ContainsKey p1 && board.Moves.[p1] = player, board.Moves.ContainsKey p2 && board.Moves.[p2] = player) with
        | (true, true) -> Some(p1, p2, dir)
        | (true, false) -> Some(p1, (row, col), dir)
        | (false, true) -> Some((row, col), p2, dir)
        | (false, false) -> None

    let newRows = Seq.choose generateNewRows possibleRows

    let mergedRows =
        let mutable rows = board.Rows
        for (pFrom, pTo, dir) in newRows do
            let extendedFrom = 
                match getRow pFrom dir rows with
                | Some row ->
                    rows <- nullifyRow row.To dir rows
                    row.From
                | None -> pFrom
            let extendedTo = 
                match getRow pTo dir rows with
                | Some row ->
                    rows <- nullifyRow row.From dir rows
                    row.To
                | None -> pTo
            let newRow = Row.createRanked extendedFrom extendedTo dir newMoves
            rows <- setRow extendedFrom dir newRow rows
            rows <- setRow extendedTo dir newRow rows
        rows

    let possibleAffectedRows =
        seq { yield (row - 1, col - 1), SE
              yield (row + 1, col + 1), SE
              yield (row - 1, col), S
              yield (row + 1, col), S
              yield (row - 1, col + 1), SW
              yield (row + 1, col - 1), SW
              yield (row, col - 1), E
              yield (row, col + 1), E }

    let rankedRows =
        let mutable rows = mergedRows
        for pos, dir in possibleAffectedRows do
            match getRow pos dir rows with
            | Some row ->
                let updatedRow = Row.updateRank dir newMoves row
                rows <- setRow row.From dir updatedRow rows
                rows <- setRow row.To dir updatedRow rows
            | None -> ()
        rows

    { board with 
        Moves = newMoves
        Rows = rankedRows }

let getRows board = 
    seq { for t in board.Rows do
            let inline getRow r =
                match r with
                | Some row -> if row.From = t.Key then Some row else None
                | None -> None
            yield getRow t.Value.S
            yield getRow t.Value.E
            yield getRow t.Value.SE
            yield getRow t.Value.SW }
    |> Seq.choose (fun t -> t)
    |> Seq.toArray