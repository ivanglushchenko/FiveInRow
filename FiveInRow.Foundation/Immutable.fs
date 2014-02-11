module FiveInRow.Foundation.Immutable

open GameDef

module Row =
    type Row = { From: CellPos
                 To: CellPos
                 Length: int
                 Rank: int }

    let enumerateCells dir row =
        match dir with
        | S -> seq { for x in 0..row.Length - 1 -> fst row.From + x, snd row.From }
        | E -> seq { for x in 0..row.Length - 1 -> fst row.From, snd row.From + x }
        | SE -> seq { for x in 0..row.Length - 1 -> fst row.From + x, snd row.From + x }
        | SW -> seq { for x in 0..row.Length - 1 -> fst row.From + x, snd row.From - x }

    let create pFrom pTo =
        { From = pFrom
          To = pTo
          Length = if fst pFrom = fst pTo then 1 + (snd pFrom - snd pTo |> abs) else 1 + (fst pTo - fst pFrom)
          Rank = -1 }

    let checkCell (r, c) existingMoves = 
        if Map.containsKey (r, c) existingMoves then 0 
        else if r >= 0 && c >= 0 && r < GameDef.boardDimension && c < GameDef.boardDimension then 1
        else 0

    let getRank pFrom pTo dir existingMoves =
        match dir with
        | S -> checkCell (fst pFrom - 1, snd pFrom) existingMoves + checkCell (fst pTo + 1, snd pTo) existingMoves
        | E -> checkCell (fst pFrom, snd pFrom - 1) existingMoves + checkCell (fst pTo, snd pTo + 1) existingMoves
        | SE -> checkCell (fst pFrom - 1, snd pFrom - 1) existingMoves + checkCell (fst pTo + 1, snd pTo + 1) existingMoves
        | SW -> checkCell (fst pFrom - 1, snd pFrom + 1) existingMoves + checkCell (fst pTo + 1, snd pTo - 1) existingMoves

    let createRanked pFrom pTo dir existingMoves =
        { From = pFrom
          To = pTo
          Length = if fst pFrom = fst pTo then 1 + (snd pFrom - snd pTo |> abs) else 1 + (fst pTo - fst pFrom)
          Rank = getRank pFrom pTo dir existingMoves }

    let updateRank dir existingMoves row =
        { row with Rank = getRank row.From row.To dir existingMoves } 

module RowX =
    open Row

    type RowX = { S: Row option
                  E: Row option
                  SE: Row option
                  SW: Row option }

    let empty = { S = None
                  E = None
                  SE = None
                  SW = None }

    let isEmpty rx =
        Option.isNone rx.S || Option.isNone rx.E || Option.isNone rx.SE || Option.isNone rx.SW

    let update dir r rx =
        match dir with
        | S -> { rx with S = r }
        | E -> { rx with E = r }
        | SE -> { rx with SE = r }
        | SW -> { rx with SW = r }

    let get dir rx =
        match dir with
        | S -> rx.S
        | E -> rx.E
        | SE -> rx.SE
        | SW -> rx.SW

module Board =
    type Board = { Moves: Map<CellPos, Player>
                   Rows: Map<CellPos, RowX.RowX> }

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

    let getRowRank pos dir board =
        (getRow pos dir board.Rows |> Option.get).Rank

    let nullifyRow pos dir rows =
        let extRj = Map.find pos rows
        let newRj = RowX.update dir None extRj
        if RowX.isEmpty newRj then rows.Remove pos
        else (rows.Remove pos).Add (pos, newRj)

    let extend ((row, col), player) board =
        if board.Moves.ContainsKey (row, col) then failwith "Cell is occupied already"

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
                let newRow = Row.createRanked extendedFrom extendedTo dir board.Moves
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
                | Some row -> rows <- setRow pos dir (Row.updateRank dir rows row) rows
                | None -> ()
            rows

        { board with 
            Moves = board.Moves.Add ((row, col), player)
            Rows = rankedRows }

    let collectUniqueRows board = 
        seq { for t in board.Rows do
                yield t.Value.S
                yield t.Value.E
                yield t.Value.SE
                yield t.Value.SW }
        |> Seq.choose (fun t -> t)
        |> Set.ofSeq