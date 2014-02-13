module FiveInRow.GameMechanics.AI

open FiveInRow.GameMechanics.GameDef

type AI = { PossibleMoves: (Position * float) list
            Winner: Player option }

let empty = { PossibleMoves = []
              Winner = None }

type Forecast =
    | Mate of int
    | Check of int
    | InProgress of float

    member x.Inc turns =
        match x with
        | Mate t -> Mate(t + turns)
        | Check t -> Check(t + turns)
        | _ -> x

let getForecast histogram =
    let rowCount length rank = Array.get (Array.get histogram length) rank
    if histogram.[3].[0] > 0 || histogram.[3].[1] > 0 || histogram.[3].[2] > 0 then Mate 0
    else if rowCount 4 2 >= 1 then Mate 1
    else if rowCount 4 1 >= 2 then Mate 2
    else if rowCount 3 2 >= 2 then Mate 2
    else if rowCount 3 2 >= 1 && rowCount 4 1 >= 1 then Mate 2
    else
        let inline score length rank =
            match length + 2, rank with
            | l, _ when l > 5 -> 0.0
            | 5, _ -> float System.Int32.MaxValue
            | 4, 2 -> 64.0
            | 4, 1 -> 64.0
            | 4, 0 -> 8.0
            | 3, 2 -> 64.0
            | 3, 1 -> 64.0
            | 3, 0 -> 8.0
            | _ -> rank * rank |> float
        let sumRanks length ranks = ranks |> Array.mapi (fun rank count -> (float count) * score length rank) |> Array.sum
        InProgress(histogram |> Array.mapi sumRanks |> Array.sum)

let getEasy p board =
    let possibleMoves = Board.getUnoccupiedPositions 1 board
    let possibleBoards = possibleMoves |> Seq.map (fun pos -> Board.extend pos p board)
    let inc pl (f: Forecast) = if pl = p then f else f.Inc 1 
    let forecasts = possibleBoards |> Seq.map Board.getRowHistogram |> Seq.map (fun (h1, h2) -> inc Player1 (getForecast h1), inc Player2 (getForecast h2))
    empty