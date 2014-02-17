module FiveInRow.Core.AI

open PersistentHashMap
open GameDef
open Board
open RowHistogram

type AI = { PossibleMoves: (Position * float) list
            Winner: Player option }

let empty = { PossibleMoves = []
              Winner = None }

[<CustomEquality; CustomComparison>]
type Forecast =
    | Mate of int
    | Check of int
    | Rating of float

    member x.Inc turns =
        match x with
        | Mate t -> Mate(t + turns)
        | Check t -> Check(t + turns)
        // For mates and checks every turn decreases numerical value (returned by ToNum()) by 1%, so here we simulate the similiar effect 
        | Rating r -> Rating (r * (1.0 - 0.01 * (float turns)))
    
    member x.ToNum() =
        match x with
        | Mate t -> (100 - t) * 10000000 |> float
        | Check t -> (100 - t) * 10000 |> float
        | Rating r -> r |> float

    override x.Equals yobj =
        match yobj with
        | :? Forecast as y ->
            match x, y with
            | Mate p1, Mate p2   -> p1 = p2
            | Check p1, Check p2 -> p1 = p2
            | Rating p1, Rating p2 -> p2 = p1
            | _, _ -> false
        | _ -> false

    override x.GetHashCode() =
        match x with
        | Mate t -> (100 - t) * 10000000
        | Check t -> (100 - t) * 10000
        | Rating r -> r |> int

    interface System.IComparable with
        member x.CompareTo yobj =
            match yobj with
            | :? Forecast as y ->
                match x, y with
                | Mate p1, Mate p2   -> compare p1 p2
                | Mate p1, _         -> -1
                | _, Mate p2         -> 1
                | Check p1, Check p2 -> compare p1 p2
                | Check p1, _        -> -1
                | _, Check p2        -> 1
                | Rating p1, Rating p2 -> compare p2 p1
            | _ -> failwith "Cannot compare Forecast with other types"

let getUnoccupiedNeighbours k board =
    seq { if PersistentHashMap.length board.Moves > 0 then
            for (pos, p) in board.Moves do
                    for i in fst pos - k..fst pos + k do
                        if i >= 0 && i < boardDimension then
                            for j in snd pos - k..snd pos + k do
                                if j >= 0 && j < boardDimension && (i <> fst pos || j <> snd pos) && board.Moves.ContainsKey (i, j) = false then
                                    yield i, j
          else yield boardDimension / 2, boardDimension / 2 } |> Set.ofSeq

let getForecast player histogram =
    if RowHistogram.hasLength player 5 histogram then Mate 0
    else if RowHistogram.getCount player 4 2 histogram >= 1 then Mate 1
    else if RowHistogram.getCount player 4 1 histogram >= 2 then Mate 2
    else if RowHistogram.getCount player 3 2 histogram >= 2 then Mate 2
    else if RowHistogram.getCount player 3 2 histogram >= 1 && RowHistogram.getCount player 4 1 histogram >= 1 then Mate 2
    else
        let inline score length rank =
            match length, rank with
            | l, _ when l > 5 -> 8.0
            | 5, _ -> float System.Int32.MaxValue
            | 4, 2 -> 1024.0
            | 4, 1 -> 128.0
            | 4, 0 -> 8.0
            | 3, 2 -> 128.0
            | 3, 1 -> 32.0
            | 3, 0 -> 8.0
            | _ -> rank * rank |> float
        Rating(RowHistogram.score player score histogram)

let (+) f1 f2 =
    match f1, f2 with
    | Mate p1, Mate p2   -> Mate (min p1 p2)
    | Mate p1, _         -> Mate p1
    | _, Mate p2         -> Mate p2
    | Check p1, Check p2 -> Check (min p1 p2)
    | Check p1, _        -> Check p1
    | _, Check p2        -> Check p2
    | Rating p1, Rating p2 -> Rating (p1 + p2)

let getBoardForecast p board =
    let (f1, f2) = getForecast Player1 board.Histogram, getForecast Player2 board.Histogram
    if p = Player1 then f1 + (f2.Inc 1) else f2 + (f1.Inc 1)

let getEasy p board =
    let possibleMoves = if board.Candidates.Count = 0 then getUnoccupiedNeighbours 1 board else board.Candidates
    let possibleBoards = possibleMoves |> Seq.map (fun pos -> pos, (Board.extend pos p board), (Board.extend pos (next p) board)) |> Seq.toArray
    let possibleOutcomes = possibleBoards |> Seq.map (fun (pos, b1, b2) -> pos, getBoardForecast p b1, getBoardForecast p b2) |> Seq.toArray
    let forecasts = 
        possibleOutcomes 
        |> Seq.map (fun (pos, f1, f2) -> pos, f1 + f2)
        |> Seq.sortBy snd |> Seq.toArray
    { empty with PossibleMoves = forecasts |> Seq.map (fun (pos, f) -> pos, f.ToNum()) |> Seq.toList }