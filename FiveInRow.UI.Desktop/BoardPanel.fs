namespace FiveInRow.UI.Desktop

open System
open System.Collections.Generic
open System.Linq
open System.Text
open System.Windows.Controls
open System.Windows
open System.Windows.Input
open System.Windows.Media

open FiveInRow.Foundation

type BoardPanel() as this =
    inherit Panel()

    let mutable _cellWidth = 0.0
    let mutable _cellHeigth = 0.0

    let mutable _isCaptured = false
    let mutable _isDragging = false
    let mutable _lastPos = new Point(0.0, 0.0)
    let mutable _offset = new Vector(0.0, 0.0)
    let _tTransform = new TranslateTransform()

    let onLoaded e =
        let owner = ItemsControl.GetItemsOwner(this)
        let vm = owner.DataContext :?> MainWindowViewModel

        let onMouseDown (p: Point) =
            _isCaptured <- true
            _lastPos <- p
            ()

        let onMouseUp (p: Point) =
            _isCaptured <- false
            this.ReleaseMouseCapture()

            if _isDragging then _isDragging <- false
            else
                let c = p - _offset
                let index = (int(c.Y / _cellWidth) + 1, int(c.X / _cellHeigth) + 1)
                vm.Set index

        let onMouseMove(p: Point) =
            if _isCaptured then
                this.CaptureMouse() |> ignore
                let offset = p - _lastPos
                if _isDragging || offset.Length > 6.0 then
                    _isDragging <- true
                    _offset <- _offset + offset
                    _offset <- Vector(Math.Min(_offset.X, this.ActualWidth / 4.0), Math.Min(_offset.Y, this.ActualHeight / 4.0))
                    _tTransform.X <- _offset.X
                    _tTransform.Y <- _offset.Y
                    _lastPos <- p
                    vm.Offset <- _offset

        owner.MouseDown.Add (fun e -> owner |> e.GetPosition |> onMouseDown)
        owner.MouseUp.Add (fun e -> owner |> e.GetPosition |> onMouseUp)
        owner.MouseMove.Add (fun e -> owner |> e.GetPosition |> onMouseMove)
        ()

    do 
        this.Loaded.Add onLoaded
        this.RenderTransform <- _tTransform

    override x.MeasureOverride availableSize = 
        if x.InternalChildren.Count = 0 then base.MeasureOverride(availableSize)
        else
            for el in x.InternalChildren do
                el.Measure(availableSize)
            _cellWidth <- x.InternalChildren |> Seq.cast<FrameworkElement> |> Seq.map (fun t -> t.DesiredSize.Width) |> Seq.max
            _cellHeigth <- x.InternalChildren |> Seq.cast<FrameworkElement> |> Seq.map (fun t -> t.DesiredSize.Height) |> Seq.max
            availableSize

    override x.ArrangeOverride finalSize =
        for el in x.InternalChildren |> Seq.cast<FrameworkElement> do
            let cell = el.DataContext :?> CellView
            Rect(Point(_cellWidth * float(cell.Col - 1), _cellHeigth * float(cell.Row - 1)), Size(_cellWidth, _cellHeigth)) |> el.Arrange
        finalSize

