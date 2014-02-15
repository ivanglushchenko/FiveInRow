namespace FiveInRow.Foundation

open GameDef
open System
open FiveInRow.Core.GameDef

type Row(cells: Map<int, Map<int, Cell>>, posFrom: Position, posTo: Position) =
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

    let endPoint =
        match direction with
        | E -> snd posTo
        | _ -> fst posTo
                
    let getRank (cells: Map<int, Map<int, Cell>>) = 
        let inline add (pos: Position) dr dc = (fst pos + dr, snd pos + dc)
        let inline check pos = if isValid pos && cells.[fst pos].[snd pos].IsEmpty then 1 else 0
        let candidates = 
            match direction with
            | S -> [| add posFrom -1 0; add posTo 1 0 |]
            | E -> [| add posFrom 0 -1; add posTo 0 1 |]
            | SE -> [| add posFrom -1 -1; add posTo 1 1 |]
            | SW -> [| add posFrom -1 1; add posTo 1 -1 |]
        check candidates.[0] + check candidates.[1]

    let rank = getRank cells

    let actionPoints = lazy (
        let inline get (pos: Position) dr dc = 
            let newPos = (fst pos + dr, snd pos + dc)
            if isValid newPos && cells.[fst newPos].[snd newPos].IsEmpty then [ newPos ] else []
        match direction with
            | S -> get posFrom -1 0 @ get posTo 1 0
            | E -> get posFrom 0 -1 @ get posTo 0 1
            | SE -> get posFrom -1 -1 @ get posTo 1 1
            | SW -> get posFrom -1 1 @ get posTo 1 -1
        )

    static member Create (cells: Map<int, Map<int, Cell>>) posFrom posTo =
        match (compare (fst posFrom) (fst posTo), compare (snd posFrom) (snd posTo)) with
        | (-1, _) -> Row(cells, posFrom, posTo)
        | (1, _)  -> Row(cells, posTo, posFrom)
        | (_, -1) -> Row(cells, posFrom, posTo)
        | (_, 1)  -> Row(cells, posTo, posFrom)
        | _       -> raise (Exception())

    static member Merge (cells: Map<int, Map<int, Cell>>) (rowStart: Row) (rowEnd: Row) = Row.Create cells rowStart.From rowEnd.To

    member x.From with get() = posFrom

    member x.To with get() = posTo

    member x.Length with get() = length

    member x.Key with get() = (direction, zero)

    member x.Id with get() = { direction = direction; zero = zero; startPoint = startPoint; endPoint = endPoint }

    member x.StartPoint with get() = startPoint

    member x.EndPoint with get() = endPoint

    member x.Rank with get() = rank

    member x.ActionPoints with get() = actionPoints.Value

    override x.ToString() = sprintf "(%i:%i)->(%i:%i)" (fst posFrom) (snd posFrom) (fst posTo) (snd posTo)

    member x.UpdateRank (cells: Map<int, Map<int, Cell>>) = 
        let newRank = getRank cells
        if newRank <> rank then Some(Row.Create cells x.From x.To) else None

    member x.Extend pos =
        if neighbours posFrom pos then Row.Create cells pos posTo else Row.Create cells posFrom pos