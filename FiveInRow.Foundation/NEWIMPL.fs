module A

open System

let mutable boardDimension = 0

type Player = Player1 | Player2

let nextPlayer = function | Player1 -> Player2 | _ -> Player1

type CellValue = Empty | Occupied of Player

type CellPos = int * int

let isValid pos = fst pos > 0 && snd pos > 0 && fst pos <= boardDimension && snd pos <= boardDimension


type Cell(pos: CellPos, value: CellValue) =
    member x.Value with get() = value

    member x.Pos with get() = pos

    member x.IsEmpty with get() = match value with | Empty -> true | _ -> false

    member x.IsOccupiedBy player =
        match value with
        | Occupied(p) when p = player -> true
        | _ -> false

    override x.ToString() =
        match value with
        | Empty -> sprintf "[%i:%i-> . ]" (fst pos) (snd pos)
        | Occupied(Player1) -> sprintf "[%i:%i-> x ]" (fst pos) (snd pos)
        | Occupied(Player2) -> sprintf "[%i:%i-> o ]" (fst pos) (snd pos)
     

let neighbours (c1: CellPos) (c2: CellPos) = abs(fst c1 - fst c2) <= 1 && abs(snd c1 - snd c2) <= 1


type Direction = S | E | SE | SW

type RowKey = Direction * int

type Row(posFrom: CellPos, posTo: CellPos) =
    let length = 
        if fst posFrom = fst posTo then 1 + (snd posFrom - snd posTo |> abs)
        else 1 + (fst posTo - fst posFrom)

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
        let add (pos: CellPos) dr dc = (fst pos + dr, snd pos + dc)
        let candidates = 
            match direction with
            | S -> [ add posFrom -1 0; add posTo 1 0 ]
            | E -> [ add posFrom 0 -1; add posTo 0 1 ]
            | SE -> [ add posFrom -1 -1; add posTo 1 1 ]
            | SW -> [ add posFrom -1 1; add posTo 1 -1 ]
        rank <- candidates |> List.filter (fun pos -> isValid pos && cells.[fst pos].[snd pos].IsEmpty) |> List.length


type Fitness = 
    | Win // 5 in a row
    | WinIn1Turn  // open 4-length row
    | WinIn2Turns // mate in 2 turns
    | Probability of float 

type Board(currentPlayer: Player, cells: Map<int, Map<int, Cell>>, rows: Map<Player, Map<RowKey, Row list>>) =
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
        let nextRows = rows |> Map.map (fun player value -> if player = player then merge value else value)

        for playerRows in nextRows do
            for rowGroup in playerRows.Value do
                for row in rowGroup.Value do
                    row.ResetRank nextCells

        Board(nextPlayer player, nextCells, nextRows)

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

    static member Create dim =
        boardDimension <- dim
        let cells = Map.ofList [ for i in 1..dim -> (i, Map.ofList [ for j in 1..dim -> (j, Cell((i, j), Empty)) ]) ]
        let rows = [ Player1; Player2] |> List.map (fun p -> (p, Map.empty<RowKey, Row list>)) |> Map.ofList
        Board(Player1, cells, rows)

    member x.Set (i, j) = x.SetAs (i, j) currentPlayer

    member x.SetAs (i, j) player =
        match cells.[i].[j].Value with
        | Occupied(_) -> None
        | Empty       -> Some(cells.[i].[j] |> extendWith player)
        
    member x.Print() =
        for row in cells do
            for cell in row.Value do
                match cell.Value.Value with
                | Empty -> printf "."
                | Occupied(Player1) -> printf "x"
                | Occupied(Player2) -> printf "o"
            printfn ""

    member x.Fitness with get() = fitness.Value

    member x.CalculateBestMove() =
        let possibleMoves = 
            seq { for row in cells do
                    for cell in row.Value do
                        if cell.Value.IsEmpty then
                            if neighboursOf cell.Value |> Seq.exists (fun c -> c.IsEmpty = false) then
                                yield cell.Value }
        let opponent = nextPlayer currentPlayer
        let possibleBoards = possibleMoves |> Seq.map (fun c -> (c, x.Set c.Pos, x.SetAs c.Pos opponent))

        []

//let b0 = Board.Create(9)
//
//let moves = [   (1, 1); (1, 2); 
//                (2, 1); (1, 3); 
//                (3, 1); (4, 4);
//                (5, 1); (4, 5);
//                (6, 1) ]
//
//let exec b (moves: (int * int) list) = 
//    moves |> List.fold (fun (acc: Board) m -> acc.Set m |> Option.get) b
//
//let t1 = exec b0 moves
//t1.CalculateBestMove() |> ignore