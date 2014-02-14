module FiveInRow.GameMechanics.AI

open FiveInRow.GameMechanics.GameDef
open FiveInRow.GameMechanics.Board

type AI = { PossibleMoves: (Position * float) list
            Winner: Player option }

let empty = { PossibleMoves = []
              Winner = None }

[<StructuralEquality; CustomComparison>]
type Forecast =
    | Mate of int
    | Check of int
    | Rating of float

    member x.Inc turns =
        match x with
        | Mate t -> Mate(t + turns)
        | Check t -> Check(t + turns)
        | _ -> x
    
    member x.ToNum() =
        match x with
        | Mate t -> (100 - t) * 10000000 |> float
        | Check t -> (100 - t) * 10000 |> float
        | Rating r -> r |> float

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

let getUnoccupiedPositions k board =
    seq { if board.Moves.Count > 0 then
            for move in board.Moves do
                    for i in fst move.Key - k..fst move.Key + k do
                        if i >= 0 && i < boardDimension then
                            for j in snd move.Key - k..snd move.Key + k do
                                if j >= 0 && j < boardDimension && (i <> fst move.Key || j <> snd move.Key) && board.Moves.ContainsKey (i, j) = false then
                                    yield i, j
          else yield boardDimension / 2, boardDimension / 2 } |> Seq.distinct

let getForecast histogram =
    let rowCount length rank = Array.get (Array.get histogram (length - 2)) rank
    if histogram.[3].[0] > 0 || histogram.[3].[1] > 0 || histogram.[3].[2] > 0 then Mate 0
    else if rowCount 4 2 >= 1 then Mate 1
    else if rowCount 4 1 >= 2 then Mate 2
    else if rowCount 3 2 >= 2 then Mate 2
    else if rowCount 3 2 >= 1 && rowCount 4 1 >= 1 then Mate 2
    else
        let inline score length rank =
            match length + 2, rank with
            | l, _ when l > 5 -> 8.0
            | 5, _ -> float System.Int32.MaxValue
            | 4, 2 -> 64.0
            | 4, 1 -> 64.0
            | 4, 0 -> 8.0
            | 3, 2 -> 64.0
            | 3, 1 -> 64.0
            | 3, 0 -> 8.0
            | _ -> rank * rank |> float
        let sumRanks length ranks = ranks |> Array.mapi (fun rank count -> (float count) * score length rank) |> Array.sum
        Rating(histogram |> Array.mapi sumRanks |> Array.sum)

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
    let (h1, h2) = Board.getRowHistogram board
    let (f1, f2) = getForecast h1, getForecast h2
    if p = Player1 then f1 + (f2.Inc 1) else f2 + (f1.Inc 1)

let getEasy p board =
    let possibleMoves = getUnoccupiedPositions 1 board
    let possibleBoards = possibleMoves |> Seq.map (fun pos -> pos, (Board.extend pos p board), (Board.extend pos (next p) board))
    let forecasts = 
        possibleBoards 
        |> Seq.map (fun (pos, b1, b2) -> pos, (getBoardForecast p b1) + (getBoardForecast p b2))
        |> Seq.sortBy snd
    { empty with PossibleMoves = forecasts |> Seq.map (fun (pos, f) -> pos, f.ToNum()) |> Seq.toList }