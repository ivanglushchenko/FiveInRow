namespace FiveInRow.Foundation

open GameDef
open System

type Row(posFrom: CellPos, posTo: CellPos) =
    let length = 
        if fst posFrom = fst posTo then 1 + (snd posFrom - snd posTo |> abs)
        else 1 + (fst posTo - fst posFrom)

    let direction = 
        match (compare (fst posFrom) (fst posTo), compare (snd posFrom) (snd posTo)) with
        | (0, _) -> E
        | (_, 0) -> S
        | (1, 1) | (-1, -1) -> SE
        | _      -> SW

    let zero = 
        match direction with
        | S -> snd posFrom
        | E -> fst posFrom
        | SE -> snd posFrom - fst posFrom
        | SW -> snd posFrom + fst posFrom

    let startPoint =
        match direction with
        | E -> snd posFrom
        | _ -> fst posFrom
                
    let mutable rank = 0

    static member Create posFrom posTo =
        match (compare (fst posFrom) (fst posTo), compare (snd posFrom) (snd posTo)) with
        | (-1, _) -> Row(posFrom, posTo)
        | (1, _)  -> Row(posTo, posFrom)
        | (_, -1) -> Row(posFrom, posTo)
        | (_, 1)  -> Row(posTo, posFrom)
        | _       -> raise (Exception())

    static member Merge (rowStart: Row) (rowEnd: Row) = Row.Create rowStart.From rowEnd.To

    member x.From with get() = posFrom

    member x.To with get() = posTo

    member x.Length with get() = length

    member x.Key with get() = (direction, zero)

    member x.StartPoint with get() = startPoint

    member x.Rank with get() = rank

    override x.ToString() = sprintf "(%i:%i)->(%i:%i)" (fst posFrom) (snd posFrom) (fst posTo) (snd posTo)

    member x.ResetRank (cells: Map<int, Map<int, Cell>>) = 
        let inline add (pos: CellPos) dr dc = (fst pos + dr, snd pos + dc)
        let inline check pos = if isValid pos && cells.[fst pos].[snd pos].IsEmpty then 1 else 0
        let candidates = 
            match direction with
            | S -> [| add posFrom -1 0; add posTo 1 0 |]
            | E -> [| add posFrom 0 -1; add posTo 0 1 |]
            | SE -> [| add posFrom -1 -1; add posTo 1 1 |]
            | SW -> [| add posFrom -1 1; add posTo 1 -1 |]
        rank <- check candidates.[0] + check candidates.[1]