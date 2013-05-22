module NEWIMPL

open System


type Player = Player1 | Player2

let nextPlayer = function | Player1 -> Player2 | _ -> Player1

type CellValue = Empty | Occupied of Player

type CellPos = int * int

type Cell(pos: CellPos) =
    let mutable _value = Empty

    member x.Value
        with get() = _value
        and set(v) = _value <- v

    member x.Pos with get() = pos

    member x.IsEmpty with get() = match _value with | Empty -> true | _ -> false

    member x.IsOccupiedBy player =
        match x.Value with
        | Occupied(p) when p = player -> true
        | _ -> false

let neighbours (c1: CellPos) (c2: CellPos) = abs(fst c1 - fst c2) <= 1 && abs(snd c1 - snd c2) <= 1


type Direction = S | E | SE | SW

type RowKey = Direction * int

type Row(posFrom: CellPos, posTo: CellPos) =
    let length = 
        if fst posFrom = fst posTo then snd posFrom - snd posTo |> abs
        else fst posTo - fst posFrom

    let direction = 
        match (compare (fst posFrom) (fst posTo), compare (snd posFrom) (snd posTo)) with
        | (0, _) -> E
        | (_, 0) -> S
        | (1, 1) -> SE
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


let mutable cells = Array.empty<Cell[]>
let mutable rows = Map.empty<Player, Map<RowKey, Row list>>

type Board(nextTurn: Player) =
    let neighboursOf (cell: Cell) = 
        seq { for i in fst cell.Pos - 1..fst cell.Pos + 1 do
                for j in snd cell.Pos - 1..snd cell.Pos + 1 do
                    if i > 0 && j > 0 && i <= cells.Length && j <= cells.Length && (i <> fst cell.Pos || j <> snd cell.Pos) then
                        yield cells.[i - 1].[j - 1] }

    let extendWith (cell: Cell) = 
        let nearbyCells = neighboursOf cell |> Seq.filter (fun c -> c.IsOccupiedBy nextTurn) |> List.ofSeq
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

        rows <- rows |> Map.map (fun player value -> if player = nextTurn then merge value else value)

        cell.Value <- Occupied(nextTurn)

        Board(nextPlayer nextTurn)

    static member Create dim =
        cells <- [| for i in 1..dim -> [| for j in 1..dim -> Cell(i, j) |] |]
        rows <-  [ Player1; Player2] |> List.map (fun p -> (p, Map.empty<RowKey, Row list>)) |> Map.ofList
        Board(Player1)

    member x.Set i j =
        match cells.[i - 1].[j - 1].Value with
        | Occupied(_) -> None
        | Empty       -> Some(cells.[i - 1].[j - 1] |> extendWith)
        
    member x.Print() =
        for row in cells do
            for cell in row do
                match cell.Value with
                | Empty -> printf "."
                | Occupied(Player1) -> printf "x"
                | Occupied(Player2) -> printf "o"
            printfn ""


