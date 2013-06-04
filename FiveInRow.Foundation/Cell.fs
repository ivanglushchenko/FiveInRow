namespace FiveInRow.Foundation

open GameDef
open System.ComponentModel

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

    static member Neighbours (cell: Cell) (cells: Map<int, Map<int, Cell>>) =
        seq { for i in fst cell.Pos - 1..fst cell.Pos + 1 do
                for j in snd cell.Pos - 1..snd cell.Pos + 1 do
                    if isValid (i, j) && (i <> fst cell.Pos || j <> snd cell.Pos) then
                        yield cells.[i].[j] }