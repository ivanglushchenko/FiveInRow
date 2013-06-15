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

    static member K_Neighbours (pos: CellPos) (cells: Map<int, Map<int, Cell>>) k =
        seq { for i in fst pos - k..fst pos + k do
                for j in snd pos - k..snd pos + k do
                    if isValid (i, j) && (i <> fst pos || j <> snd pos) then
                        yield cells.[i].[j] }

    static member Neighbours (pos: CellPos) (cells: Map<int, Map<int, Cell>>) = Cell.K_Neighbours pos cells 1