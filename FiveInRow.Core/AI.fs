module FiveInRow.Core.AI

open PersistentHashMap
open GameDef
open Board
open RowHistogram

type AI = { PossibleMoves: (Point * float) list }

let empty = { PossibleMoves = [] }

[<CustomEquality; CustomComparison>]
type Forecast =
    | Mate of int
    | Check of int
    | Rating of float
    
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

    override x.ToString() =
        match x with
        | Mate t -> sprintf "Mate %i" t
        | Check t -> sprintf "Chck %i" t
        | Rating t -> sprintf "R %O" t

let inline isCheckOrMate f =
    match f with
    | Mate _ -> true
    | Check _ -> true
    | _ -> false

let inline isMate f =
    match f with
    | Mate _ -> true
    | _ -> false

let inline scoreEasy length rank =
    match length, rank with
    | l, _ when l > 5 -> 8.0
    | 5, _ -> float System.Int32.MaxValue
    | 4, 2 -> 64.0
    | 4, 1 -> 32.0
    | 4, 0 -> 8.0
    | 3, 2 -> 64.0
    | 3, 1 -> 32.0
    | 3, 0 -> 8.0
    | _ -> rank * rank |> float

let inline scoreMedium length rank =
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

let inline scoreHard length rank =
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

let getUnoccupiedNeighbours k board =
    seq { if PersistentHashMap.length board.Moves > 0 then
            for (pos, p) in board.Moves do
                    for i in fst pos - k..fst pos + k do
                        if i >= 0 && i < boardDimension then
                            for j in snd pos - k..snd pos + k do
                                if j >= 0 && j < boardDimension && (i <> fst pos || j <> snd pos) && board.Moves.ContainsKey (i, j) = false then
                                    yield i, j
          else yield boardDimension / 2, boardDimension / 2 } |> Set.ofSeq

let getForecast scorer player histogram =
    if RowHistogram.hasLength player 5 histogram then Mate 0
    else if RowHistogram.getCount player 4 2 histogram >= 1 then Mate 1
    else if RowHistogram.getCount player 4 1 histogram >= 2 then Mate 2
    else if RowHistogram.getCount player 3 2 histogram >= 2 then Mate 2
    else if RowHistogram.getCount player 3 2 histogram >= 1 && RowHistogram.getCount player 4 1 histogram >= 1 then Mate 2
    else if RowHistogram.getCount player 4 1 histogram >= 1 then Check 1
    else if RowHistogram.getCount player 3 2 histogram >= 1 then Check 2
    else Rating(RowHistogram.score player scorer histogram)

let getTopChoices getForecast s =
    let sortedOutcomes = s |> Seq.sortBy getForecast
    if Seq.isEmpty sortedOutcomes then sortedOutcomes
    else
        let referenceItem = Seq.head sortedOutcomes
        seq { yield referenceItem
              for item in Seq.skip 1 sortedOutcomes do
                 if getForecast referenceItem = getForecast item then yield item } |> shuffle

let getBestMovesByScore scorer discountRate p board =
    let possibleMoves = (if board.Candidates.Count = 0 then getUnoccupiedNeighbours 1 board else board.Candidates)
    let possibleBoards = possibleMoves |> Seq.map (fun pos -> pos, (Board.extend pos p board), (Board.extend pos (next p) board))
    let possibleOutcomes = possibleBoards |> Seq.map (fun (pos, b1, b2) -> pos, RowHistogram.score p scorer b1.Histogram, RowHistogram.score (next p) scorer b2.Histogram)
    let forecasts = 
        possibleOutcomes 
        |> Seq.map (fun (pos, f1, f2) -> pos, f1 + f2 * discountRate)
        |> getTopChoices (fun (p, f) -> -f)
    { empty with PossibleMoves = forecasts |> Seq.map (fun (pos, f) -> pos, f) |> Seq.toList }

type StrategyNode =
    | Outcome of Point * Forecast
    | Fork of (Point * StrategyNode) array
    | Inconclusive

    override x.ToString() =
        match x with
        | Outcome (p, f) -> sprintf "Out %O" f
        | Fork c ->  sprintf "Fork %i" c.Length
        | Inconclusive -> "?"

let rec consolidate n =
    match n with
    | Outcome (p, f) -> Some f
    | Fork t ->
        let options = t |> Seq.choose (snd >> consolidate) |> Seq.sort
        if Seq.isEmpty options then None
        else Seq.head options |> Some
    | Inconclusive -> None

let getCheckMates p board =
    let possibleMoves = (if board.Candidates.Count = 0 then getUnoccupiedNeighbours 1 board else board.Candidates)
    let possibleBoards = possibleMoves |> Seq.map (fun pos -> pos, Board.extend pos p board)
    let possibleOutcomes = possibleBoards |> Seq.map (fun (pos, b) -> pos, b, getForecast scoreHard p b.Histogram)
    possibleOutcomes |> Seq.filter (trd >> isCheckOrMate)

let playCounterMoves p board =
    getCheckMates p board 
    |> Seq.filter (fun (_, _, f) -> match f with | Mate p -> p <= 1 | _ -> false)
    |> Seq.fold (fun s (pos, _, _) -> Board.extend pos (next p) s) board

let rec buildStrategyTree p level upperBound board =
    let inline filterByUpperBound t = 
        match upperBound with
        | Some f -> t < f
        | _ -> true
    if level = 0 then Inconclusive
    else
        let checkMates = getCheckMates p board 
        let (mates, checks) = 
            checkMates
            |> Seq.filter (trd >> filterByUpperBound) 
            |> Seq.toList 
            |> List.partition (trd >> isMate)
        if mates.IsEmpty = false then
            // There are mates that we can pursue, so let's ignore opponent's threat
            mates
            |> List.sortBy trd
            |> List.head
            |> fun (pos, _, f) -> Outcome (pos, f)
        else if checks.IsEmpty = false then
            // We have checks that are better than opponent's, so let's be aggressive
            checks 
            |> Seq.map (fun (pos, b , f) -> pos, b |> playCounterMoves p |> buildStrategyTree p (level - 1) upperBound) 
            |> Seq.toArray 
            |> Fork
        else
            // Let's react to one of the top opponent's threats
            let topThreats = 
                getCheckMates (next p) board 
                |> getTopChoices (fun (pos, _, f) -> f)
            if Seq.isEmpty topThreats then Inconclusive
            else
                let evaluateFor pos =
                    Board.extend pos p board 
                    |> buildStrategyTree p (level - 1) None 
                    |> consolidate 
                    |> Option.bind (fun v -> Some (pos, v))
                let consolidatedOptions = 
                    topThreats 
                    |> Seq.choose (fun (pos, _, _) -> evaluateFor pos)
                    |> Seq.sortBy snd
                if Seq.isEmpty consolidatedOptions then Inconclusive
                else consolidatedOptions |> Seq.head |> Outcome

let getEasy p board =
    getBestMovesByScore scoreEasy 0.75 p board

let getMedium p board =
    getBestMovesByScore scoreMedium 0.5 p board

let getHard p board =
    let inline toBound f = if isCheckOrMate f then Some f else None
    let getBestMove p level upperBound = 
        match buildStrategyTree p level upperBound board with
        | Outcome (p, f) -> Some (p, f)
        | Fork t ->
            let consolidatedOptions = 
                t 
                |> Seq.choose (fun (pos, s) -> consolidate s |> Option.bind (fun v -> Some (pos, v))) 
                |> Seq.sortBy snd
            if Seq.isEmpty consolidatedOptions then None else Seq.head consolidatedOptions |>Some
        | _ -> None
    let myForecast, opForecast = getForecast scoreHard p board.Histogram, getForecast scoreHard (next p) board.Histogram
    let myBestMove, opBestMove = getBestMove p 2 (toBound opForecast), getBestMove (next p) 1 (toBound myForecast)
    let bestMove = 
        match myBestMove, opBestMove with
        | Some (p1, f1), Some (p2, f2) -> if f1 > f2 then Some p2 else Some p1
        | Some (p1, _), _ -> Some p1
        | _, Some (p2, _) -> Some p2
        | _ -> None
    match bestMove with
    | Some p -> { PossibleMoves = [ p, 1.0 ] }
    | None -> getBestMovesByScore scoreHard 0.5 p board

let getImpossible p board =
    getMedium p board
    
let get = function
    | Easy -> getEasy
    | Medium -> getMedium
    | Hard -> getHard
    | Impossible -> getImpossible