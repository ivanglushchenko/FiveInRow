module FiveInRow.Core.Row

open GameDef
open PersistentHashMap

type Row = 
    { From: Position
      To: Position
      Length: int
      Rank: int }

    override x.ToString() =
        sprintf "%i:%i -> %i:%i, r%i" (fst x.From + 1) (snd x.From + 1) (fst x.To + 1) (snd x.To + 1) x.Rank

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
    if PersistentHashMap.containsKey (r, c) existingMoves then 0 
    else if isValid (r, c) then 1
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