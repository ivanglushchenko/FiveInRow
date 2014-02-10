module FiveInRow.Foundation.Immutable

open GameDef

type Row = { From: CellPos
             To: CellPos
             Length: int
             Rank: int }

type Rows = { S: Row option
              E: Row option
              SE: Row option
              SW: Row option }

type Board = { Moves: Map<CellPos, Player>
               Rows: Map<CellPos, Rows> }

let EmptyBoard = { Moves = Map.empty
                   Rows = Map.empty } 

let getRows pos board = if board.Rows.ContainsKey pos then Some board.Rows.[pos] else None

let getRow dir rows =
    match dir with
    | S -> rows.S
    | E -> rows.E
    | SE -> rows.SE
    | SW -> rows.SW

let enumerateCells row dir =
    match dir with
    | S -> seq { for x in 0..row.Length - 1 -> fst row.From + x, snd row.From }
    | E -> seq { for x in 0..row.Length - 1 -> fst row.From, snd row.From + x }
    | SE -> seq { for x in 0..row.Length - 1 -> fst row.From + x, snd row.From + x }
    | SW -> seq { for x in 0..row.Length - 1 -> fst row.From + x, snd row.From - x }

let createRow pFrom pTo =
    { From = pFrom
      To = pTo
      Length = if fst pFrom = fst pTo then 1 + (snd pFrom - snd pTo |> abs) else 1 + (fst pTo - fst pFrom)
      Rank = -1 }

let createRowWithRank (pFrom, pTo) dir existingMoves =
    let inline checkCell p = if Map.containsKey p existingMoves then 0 else 1
    { From = pFrom
      To = pTo
      Length = if fst pFrom = fst pTo then 1 + (snd pFrom - snd pTo |> abs) else 1 + (fst pTo - fst pFrom)
      Rank = match dir with
             | S -> checkCell (fst pFrom - 1, snd pFrom) + checkCell (fst pTo + 1, snd pTo)
             | E -> checkCell (fst pFrom, snd pFrom - 1) + checkCell (fst pTo, snd pTo + 1)
             | SE -> checkCell (fst pFrom - 1, snd pFrom - 1) + checkCell (fst pTo + 1, snd pTo + 1)
             | SW -> checkCell (fst pFrom - 1, snd pFrom + 1) + checkCell (fst pTo + 1, snd pTo - 1) }

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

    let newRows = 
        possibleRows 
        |> Seq.choose generateNewRows

    let extendedRows =
        seq { for (pFrom, pTo, dir) in newRows do
                let extendedPoints =
                    let fromRow = getRows pFrom board |> Option.bind (getRow dir)
                    let extendedFrom = 
                        match fromRow with
                        | Some row -> row.From
                        | None -> pFrom
                    let toRow = getRows pTo board |> Option.bind (getRow dir)
                    let extendedTo =
                        match toRow with
                        | Some row -> row.To
                        | None -> pTo
                    extendedFrom, extendedTo
                yield createRowWithRank extendedPoints dir board.Moves, dir }

    let mergedRows =
        let mutable rows = board.Rows
        for (row, dir) in extendedRows do
            ()
        rows

    { board with 
        Moves = board.Moves.Add ((row, col), player)
        Rows = mergedRows }