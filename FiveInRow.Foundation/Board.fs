namespace FiveInRow.Foundation

open System
open System.ComponentModel

type Board(cols:int, rows:int, vertices: Cell list, nextValue: CellValue) =
    inherit ObservableObject()

    let mutable _value = nextValue
    let _map = vertices |> List.map (fun t -> (t.Index, t)) |> Map.ofList
    let mutable _graphX = new Graph([], [], _map)
    let mutable _graphO = new Graph([], [], _map)
    let mutable _controlsXPlayer = true
    let mutable _controlsOPlayer = false

    new(cols, rows, vertices) = Board(cols, rows, vertices, X)

    member x.Colums with get() = cols

    member x.Rows with get() = rows

    member x.Map with get() = _map

    member x.NextValue 
        with get() = _value
        and  set(v) =
            _value <- v
            x.OnPropertyChanged(<@ x.NextValue @>)

    member x.ControlsOPlayer 
        with get() = _controlsOPlayer
        and  set(v) =
            _controlsOPlayer <- v
            x.OnPropertyChanged(<@ x.ControlsOPlayer @>)

    member x.ControlsXPlayer 
        with get() = _controlsXPlayer
        and  set(v) =
            _controlsXPlayer <- v
            x.OnPropertyChanged(<@ x.ControlsXPlayer @>)

    member x.Cells with get() = vertices

    static member CreateNew(cols:int, rows:int) =
        let vertices = 
            let getNeighbors c r =
                [ for i in c - 1 .. c + 1 do
                  for j in r - 1 .. r + 1 do
                  if i > 0 && j > 0 && i <= cols && j <= rows then yield (i, j) ]
            [ for c in 1..cols do
              for r in 1..rows do
              yield (c,r) ]
            |> List.map (fun (i, j) -> new Cell((i, j), getNeighbors i j, Unset))
        new Board(cols, rows, vertices)

    member x.Set(i: (int * int)) =
        let v = _map.[i]
        if v.IsEmpty() then
            v.Set(_value)
            match _value with 
            | X -> 
                _graphX <- _graphX.Extend(v)
                _graphO.RefreshDegreesOfFreedom()
                x.NextValue <- O
                if _graphX.IsTheWinner() then Some(X) else if x.ControlsOPlayer = false then x.MakeAIMove() else None
            | O -> 
                _graphO <- _graphO.Extend(v)
                _graphX.RefreshDegreesOfFreedom()
                x.NextValue <- X
                if _graphO.IsTheWinner() then Some(O) else if x.ControlsXPlayer = false then x.MakeAIMove() else None
            | _ -> raise (NotSupportedException())
        else
            None

    member x.MakeAIMove() =
        let nextMove = 
            match _value with
            | X -> 
                _graphX.MarkPossibleChoices(_graphO)
            | O ->
                _graphO.MarkPossibleChoices(_graphX);
            | _ -> raise (NotSupportedException())
        x.Set(nextMove.Index)

    member x.HighlightBestMoves() =
        match _value with
        | X ->
            _graphX.MarkPossibleChoices(_graphO) |> ignore
            _graphX
        | O ->
            _graphO.MarkPossibleChoices(_graphX) |> ignore
            _graphO
        | _ -> raise (NotSupportedException())
         
    member x.Graph with get() = match _value with | X -> _graphX | _ -> _graphO

    member x.Clear() =
        for v in vertices do
            v.Set(Unset)
        _value <- X
        _graphX <- new Graph([], [], _map)
        _graphO <- new Graph([], [], _map)