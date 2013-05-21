namespace FiveInRow.Foundation

open System
open Microsoft.FSharp.Collections

type Fitness = 
    // 5 in a row
    | Win
    // open 4-length row
    | WinIn1Turn
    // mat in 2 turns
    | WinIn2Turns
    // nothing special
    | Probability of float

type Graph(vertices: Cell list, rows: Row list, map: Map<(int*int), Cell>) =
    let areNeighbors (v1: Cell) (v2: Cell) =
        let (c1, r1) = v1.Index
        let (c2, r2) = v2.Index
        Math.Abs(c1 - c2) <= 1 && Math.Abs(r1 - r2) <= 1

    member x.Rows with get() = rows

    member x.Extend(v: Cell) = 
        let newRows = 
            [ for vTo in vertices do
              if areNeighbors v vTo then if v.Index < vTo.Index then yield Row.Create(v, vTo, map) else yield Row.Create(vTo, v, map) ]
        let compactify (rows: Row seq) =
            let sortedRows = rows |> Seq.sortBy (fun t -> fst t.From.Index) |> Seq.toList
            let rec merge (list: Row list) (prev: Row) =
                match list with
                | hd::tl -> 
                    if (areNeighbors prev.To hd.From) then
                        merge tl (Row.Create(prev.From, hd.To, map))
                    else if (areNeighbors prev.From hd.To) then
                        merge tl (Row.Create(prev.To, hd.From, map))
                    else
                        prev :: merge tl hd 
                | [] -> [ prev ]
            merge sortedRows.Tail sortedRows.Head |> List.toSeq
        let mergedRows = 
            newRows @ rows
            |> Seq.groupBy (fun t -> (t.Direction, t.Zero)) 
            |> Seq.map (snd >> compactify)
            |> Seq.concat 
            |> Seq.toList
        for row in mergedRows do
            row.RefreshDegreesOfFreedom()
        new Graph(v :: vertices, mergedRows, map)

    member x.RefreshDegreesOfFreedom() =
        for row in rows do
            row.RefreshDegreesOfFreedom()

    member x.IsTheWinner() =
        rows |> List.exists (fun t -> t.Length >= 5)

    member x.Fitness =
        let getRowCount length degreesOfFreedom =
            (rows |> List.filter (fun t -> t.Length >= length && t.DegreesOfFreedom >= degreesOfFreedom)).Length
        if x.IsTheWinner() then
            Win
        else if getRowCount 4 2 >= 1 then
            WinIn1Turn
        else if getRowCount 4 1 >= 2 then
            WinIn2Turns
        else if getRowCount 3 2 >= 2 then
            WinIn2Turns
        else if getRowCount 3 2 >= 1 && getRowCount 4 1 >= 1 then
            WinIn2Turns
        else
            Probability(rows |> List.filter (fun t -> t.DegreesOfFreedom > 0) |> List.sumBy (fun t -> Math.Pow((float)t.Length * 2.0, (float)t.DegreesOfFreedom + 1.0)))

    member x.MarkPossibleChoices(opponent: Graph) =
        let (emptyCells, occupiedCells) = map |> Map.partition (fun k v -> v.IsEmpty())
        let choices = seq { for vFrom in occupiedCells do
                            for vTo in emptyCells do
                            if areNeighbors vFrom.Value vTo.Value then yield vTo } 
                      |> Seq.distinctBy (fun t -> t.Key) 
                      |> Seq.map (fun t -> t.Value)
                      |> Seq.toList
        let allChoices = choices |> List.map (fun t -> (t, x.Extend(t).Fitness, opponent.Extend(t).Fitness))
        let myWins = allChoices |> List.filter (fun (v, f1, f2) -> match f1 with | Probability p -> false | _ -> true) |> List.map (fun (v, f1, f2) -> f1) |> Set.ofList
        let opWins = allChoices |> List.filter (fun (v, f1, f2) -> match f2 with | Probability p -> false | _ -> true) |> List.map (fun (v, f1, f2) -> f2) |> Set.ofList

        let noMyWin = myWins.Contains Win = false
        let noOpWin = opWins.Contains Win = false
        let noWin = noMyWin && noOpWin
        let noMyWinIn1Turn = myWins.Contains WinIn1Turn = false
        let noOpWinIn1Turn = opWins.Contains WinIn1Turn = false
        let noWinIn1Turn = noMyWinIn1Turn && noOpWinIn1Turn
        let noMyWinIn2Turns = myWins.Contains WinIn2Turns = false
        let noOpWinIn2Turns = opWins.Contains WinIn2Turns = false
        let noWinIn2Turns = noMyWinIn2Turns && noOpWinIn2Turns

        let combine f1 f2 =
            match (f1, f2) with
            | (Win, _)                                                                     -> Double.PositiveInfinity
            | (_, Win)                       when noMyWin                                  -> Double.PositiveInfinity
            | (WinIn1Turn, _)                when noWin                                    -> Double.PositiveInfinity
            | (_, WinIn1Turn)                when noWin && noMyWinIn1Turn                  -> Double.PositiveInfinity
            | (WinIn2Turns, _)               when noWin && noWinIn1Turn                    -> Double.PositiveInfinity
            | (_, WinIn2Turns)               when noWin && noWinIn1Turn && noMyWinIn2Turns -> Double.PositiveInfinity
            | _ -> (match f1 with | Probability p -> p | _ -> 0.0) + (match f2 with | Probability p -> p | _ -> 0.0)

        for (v, f1, f2) in allChoices do
            v.Set(AI_UnderConsideration)
            v.SetProbability(combine f1 f2)

        allChoices |> List.map (fun (v, f1, f2) -> v) |> List.maxBy (fun v -> v.Probability)
