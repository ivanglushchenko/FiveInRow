module FiveInRow.Core.Board

open GameDef
open Row
open RowHistogram
open RowX
open PersistentHashMap

type Board = { Moves: PersistentHashMap<Position, Player>
               Rows: PersistentHashMap<Position, RowX>
               Histogram: RowHistogram
               Candidates: Set<int * int> }

let empty =  { Moves = PersistentHashMap.empty
               Rows = PersistentHashMap.empty
               Histogram = RowHistogram.create()
               Candidates = Set.empty } 

let getRow pos dir rows =
    if PersistentHashMap.containsKey pos rows then RowX.get dir rows.[pos]
    else None

let setRow pos dir row rows =
    if PersistentHashMap.containsKey pos rows then
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
    let extRj = PersistentHashMap.find pos rows
    let newRj = RowX.update dir None extRj
    if RowX.isEmpty newRj then rows.Remove pos
    else (rows.Remove pos).Add (pos, newRj)

let extend (r, c) player board =
    if board.Moves.ContainsKey (r, c) then failwith "Cell is occupied already"

    if isTracingEnabled then
        System.Diagnostics.Debug.WriteLine(sprintf "Board.extend %O %O %O" player r c)

    let newHistogram = RowHistogram.clone board.Histogram
    let newMoves = board.Moves.Add ((r, c), player)

    let possibleRows =
        seq { yield (r, c - 1), (r, c + 1), E
              yield (r - 1, c - 1), (r + 1, c + 1), SE
              yield (r - 1, c), (r + 1, c), S
              yield (r - 1, c + 1), (r + 1, c - 1), SW }

    let prolongate rows currentPoint desiredPoint dir getNearPoint getFarPoint =
        if board.Moves.ContainsKey desiredPoint then
            let dpPlayer = board.Moves.[desiredPoint]
            let dpRow = getRow desiredPoint dir rows
            if dpPlayer = player then
                match dpRow with
                | Some existingRow ->
                    // Extend the existing row 
                    let newRows = nullifyRow (getNearPoint existingRow) dir rows
                    RowHistogram.dec player existingRow.Length existingRow.Rank newHistogram
                    newRows, getFarPoint existingRow, true
                | None ->
                    // Create a new row by linking the existing move to the new one
                    rows, desiredPoint, false
            else
                match dpRow with
                | Some existingRow ->
                    // Reassign rank tp oponent's row
                    RowHistogram.dec dpPlayer existingRow.Length existingRow.Rank newHistogram
                    let updatedRow = Row.updateRank dir newMoves existingRow
                    if keepDeadRows || updatedRow.Rank > 0 then
                        let newRows = setRow existingRow.From dir updatedRow rows
                        let newRows = setRow existingRow.To dir updatedRow newRows
                        RowHistogram.inc dpPlayer updatedRow.Length updatedRow.Rank newHistogram
                        newRows, currentPoint, false
                    else
                        // Do not keep rank-0 rows
                        let newRows = nullifyRow existingRow.From dir rows
                        let newRows = nullifyRow existingRow.To dir newRows
                        newRows, currentPoint, false
                | None ->
                    rows, currentPoint, false
        else
            rows, currentPoint, false

    let newRows = 
        let mutable rows = board.Rows
        for (pFrom, pTo, dir) in possibleRows do
            let newRows, extendedFrom, hitFrom =  prolongate rows (r, c) pFrom dir (fun r -> r.To) (fun r -> r.From)
            let newRows, extendedTo, hitTo = prolongate newRows (r, c) pTo dir (fun r -> r.From) (fun r -> r.To)
            rows <- newRows
            if extendedFrom <> extendedTo then
                let newRow = Row.createRanked extendedFrom extendedTo dir newMoves
                if keepDeadRows || newRow.Rank > 0 then
                    rows <- setRow extendedFrom dir newRow rows
                    rows <- setRow extendedTo dir newRow rows
                    RowHistogram.inc player newRow.Length newRow.Rank newHistogram
                else
                    if extendedFrom <> pFrom && hitFrom then
                        rows <- nullifyRow extendedFrom dir rows
                    if extendedTo <> pTo && hitTo then
                        rows <- nullifyRow extendedTo dir rows
        rows

    let newCandidates =
        let mutable candidates = if board.Candidates.Contains (r, c) then board.Candidates.Remove (r, c) else board.Candidates
        for (pFrom, pTo, dir) in possibleRows do
            if isValid pFrom && newMoves.ContainsKey pFrom = false then
                candidates <- candidates.Add pFrom
            if isValid pTo && newMoves.ContainsKey pTo = false then
                candidates <- candidates.Add pTo
        candidates

    { Moves = newMoves
      Rows = newRows
      Histogram = newHistogram
      Candidates = newCandidates }

let getRows board = 
    seq { for (pos, rx) in board.Rows do
            let inline getRow r =
                match r with
                | Some row -> if row.From = pos then Some row else None
                | None -> None
            yield getRow rx.S
            yield getRow rx.E
            yield getRow rx.SE
            yield getRow rx.SW }
    |> Seq.choose (fun t -> t)

let getRowsCount board =
    getRows board |> Seq.length
                        
let replay moves =
    let rec exec moves p b =
        match moves with
        | hd :: tl -> extend hd p b |> exec tl (next p)
        | [] -> b, p
    exec moves Player1 empty

let print board =
    let sb = System.Text.StringBuilder()
    sb.AppendLine(sprintf "Board: moves: %i" board.Moves.Length) |> ignore
    for r in 0..boardDimension - 1 do
        for c in 0..boardDimension - 1 do
            if board.Moves.ContainsKey (r, c) then
                match board.Moves.[r, c] with
                | Player1 -> sb.Append "x" |> ignore
                | Player2 -> sb.Append "o" |> ignore
            else
                sb.Append "." |> ignore
        sb.AppendLine() |> ignore
    sb.ToString()

let inline getWinner board =
    if RowHistogram.hasLength Player1 5 board.Histogram then Some Player1
    else if RowHistogram.hasLength Player2 5 board.Histogram then Some Player2
    else None