namespace FiveInRow.Foundation

open System

type Direction =
    | N
    | NE
    | E
    | SE

type Row(direction: Direction, vFrom: Cell, vTo: Cell, map: Map<(int*int), Cell>) =
    let _zero = 
        match direction with
        | N -> fst vFrom.Index
        | NE -> fst vFrom.Index + snd vFrom.Index
        | E -> snd vFrom.Index
        | SE -> fst vFrom.Index - snd vFrom.Index

    let _length = 
        match direction with
        | N | NE -> Math.Abs(snd vFrom.Index - snd vTo.Index) + 1
        | E | SE -> Math.Abs(fst vFrom.Index - fst vTo.Index) + 1

    let mutable _degreesOfFreedom = -1

    static member Create(v1: Cell, v2: Cell, map: Map<(int*int), Cell>) = 
        let direction = 
            let cFrom, rFrom = v1.Index
            let cTo, rTo = v2.Index
            if cFrom = cTo && rFrom = rTo then failwith "Bad row"
            if cFrom = cTo then
                N
            else
                if rFrom = rTo then
                    E
                else
                    if (cFrom - cTo) * (rFrom - rTo) > 0 then
                        SE
                    else
                        NE
        match direction with
        | N | NE -> if snd v1.Index > snd v2.Index then new Row(direction, v2, v1, map) else new Row(direction, v1, v2, map)
        | E | SE -> if fst v1.Index > fst v2.Index then new Row(direction, v2, v1, map) else new Row(direction, v1, v2, map)

    member x.From with get() = vFrom

    member x.To with get() = vTo

    member x.Direction with get() = direction

    member x.Zero with get() = _zero

    member x.Width with get() = fst vTo.Index - fst vFrom.Index

    member x.Height with get() = snd vTo.Index - snd vFrom.Index

    member x.Length with get() = _length

    member x.DegreesOfFreedom with get() = _degreesOfFreedom

    member x.RefreshDegreesOfFreedom() = 
        let check (x, y) =
            if map.ContainsKey(x, y) then 
                if map.[(x, y)].IsEmpty() then 1 else 0
            else 0
        _degreesOfFreedom <-
            match direction with
            | N -> 
                if snd vFrom.Index < snd vTo.Index then
                    check (fst vFrom.Index, snd vFrom.Index - 1) + check (fst vTo.Index, snd vTo.Index + 1)
                else
                    check (fst vTo.Index, snd vTo.Index - 1) + check (fst vFrom.Index, snd vFrom.Index + 1)
            | NE -> 
                if fst vFrom.Index < fst vTo.Index then
                    check (fst vFrom.Index - 1, snd vFrom.Index + 1) + check (fst vTo.Index + 1, snd vTo.Index - 1)
                else
                    check (fst vTo.Index - 1, snd vTo.Index + 1) + check (fst vFrom.Index + 1, snd vFrom.Index - 1)
            | E ->
                if fst vFrom.Index < fst vTo.Index then
                    check (fst vFrom.Index - 1, snd vFrom.Index) + check (fst vTo.Index + 1, snd vTo.Index)
                else
                    check (fst vTo.Index - 1, snd vTo.Index) + check (fst vFrom.Index + 1, snd vFrom.Index)
            | SE ->
                if fst vFrom.Index < fst vTo.Index then
                    check (fst vFrom.Index - 1, snd vFrom.Index - 1) + check (fst vTo.Index + 1, snd vTo.Index + 1)
                else
                    check (fst vTo.Index - 1, snd vTo.Index - 1) + check (fst vFrom.Index + 1, snd vFrom.Index + 1)
