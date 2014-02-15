namespace FiveInRow.Foundation

open FiveInRow.Core.GameDef

type EventProc() =
    let mutable isCaptured = false
    let mutable isDragging = false
    let mutable lastPos = (0.0, 0.0)
    let mutable offset = (0.0, 0.0)
    let mutable cellWidth = 0.0
    let mutable cellHeight = 0.0

    abstract Width: float with get

    default x.Width with get() = 0.0

    abstract Height: float with get

    default x.Height with get() = 0.0

    abstract member CapturePointer: obj -> unit

    default x.CapturePointer obj = ()

    abstract member ReleasePointer: obj -> unit

    default x.ReleasePointer obj = ()

    abstract member Set: int * int -> unit

    default x.Set (row, col) = ()

    abstract member SetOffset: float * float -> unit

    default x.SetOffset (dx, dy) = ()

    member x.CellWidth
        with get() = cellWidth
        and set(v) =
            if v <> cellWidth then
                cellWidth <- v

    member x.CellHeight
        with get() = cellHeight
        and set(v) = 
            if v <> cellHeight then
                cellHeight <- v

    member x.OnPointerPressed (pointer, position) =
        isCaptured <- true
        lastPos <- position

    member x.OnPointerMoved (pointer, position) =
        if isCaptured then
            let (dx, dy) = (fst position - fst lastPos, snd position - snd lastPos)
            let d = dx * dx + dy * dy
            if isDragging = true || d > 36.0 then
                x.CapturePointer pointer
                isDragging <- true
                x.AddOffset dx dy
                lastPos <- position

    member x.OnPointerReleased (pointer, position) =
        isCaptured <- false
        x.ReleasePointer pointer

        if isDragging then
            isDragging <- false
        else
            let convert t offset size = ((t - offset) / size |> int) + 1
            let index = (convert (snd position)  (snd offset) cellHeight, convert (fst position)  (fst offset) cellWidth)
            x.Set index

    member x.AddOffset dx dy =
        let calc size origValue dv cellSize = 
            if size > 0.0 then max (-0.5 * cellSize * (float boardDimension)) (min (size * 0.5) (origValue + dv))
            else origValue + dv

        offset <- (calc x.Width (fst offset) dx cellWidth, calc x.Height (snd offset) dy cellHeight)

        x.SetOffset offset

    member x.Centrify() = x.Centrify (x.Width, x.Height)

    member x.Centrify (w, h) =
        let (dx, dy) = ((w - (float boardDimension) * x.CellWidth) / 2.0, (h - (float boardDimension) * x.CellHeight) / 2.0)
        x.AddOffset (dx - (fst offset)) (dy - (snd offset))
