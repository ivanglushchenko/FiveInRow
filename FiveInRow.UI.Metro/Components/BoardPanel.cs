using FiveInRow.Core.UI;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Windows.Foundation;
using Windows.UI.Xaml;
using Windows.UI.Xaml.Controls;
using Windows.UI.Xaml.Input;
using Windows.UI.Xaml.Media;

namespace FiveInRow.UI.Metro.Components
{
    public class BoardPanel : Panel
    {
        #region .ctors

        public BoardPanel()
        {
            Loaded += BoardPanel_Loaded;
            RenderTransformOrigin = new Point(0.5, 0.5);
            RenderTransform = _tTransform;

            _eventProc = new EventProcImpl() { Panel = this, CellWidth = 60, CellHeight = 60 };
        }

        #endregion .ctors

        #region Fields

        private EventProc _eventProc;
        private bool _isPositioned;

        private TranslateTransform _tTransform = new TranslateTransform();
        private ItemsControl _owner;
        private MainPageViewModel _vm;

        #endregion Fields

        #region Methods

        public void Centrify()
        {
            _eventProc.Centrify();
        }

        void BoardPanel_Loaded(object sender, Windows.UI.Xaml.RoutedEventArgs e)
        {
            Loaded -= BoardPanel_Loaded;

            _owner = ItemsControl.GetItemsOwner(this);
            _vm = (MainPageViewModel)_owner.DataContext;
            _vm.SetPanel(this);
            _vm.SetOffset(new Point(_tTransform.X, _tTransform.Y));

            _owner.PointerPressed += (s, arg) =>
            {
                var pos = arg.GetCurrentPoint(_owner).Position;
                _eventProc.OnPointerPressed(arg.Pointer, _eventProc.CreatePos(pos.X, pos.Y));
            };
            _owner.PointerReleased += (s, arg) =>
            {
                var pos = arg.GetCurrentPoint(_owner).Position;
                _eventProc.OnPointerReleased(arg.Pointer, _eventProc.CreatePos(pos.X, pos.Y));
            };
            _owner.PointerMoved += (s, arg) =>
            {
                var pos = arg.GetCurrentPoint(_owner).Position;
                _eventProc.OnPointerMoved(arg.Pointer, _eventProc.CreatePos(pos.X, pos.Y));
            };
        }

        protected override Size MeasureOverride(Size availableSize)
        {
            if (Children.Count == 0) return base.MeasureOverride(availableSize);
            else
            {
                foreach (var item in Children)
                {
                    item.Measure(availableSize);
                }
                return availableSize;
            }
        }

        protected override Size ArrangeOverride(Size finalSize)
        {
            if (Children.Count > 0)
            {
                if (!_isPositioned)
                {
                    _isPositioned = true;
                    _eventProc.Centrify(finalSize.Width, finalSize.Height);
                }
            }

            foreach (var item in Children.OfType<FrameworkElement>())
            {
                var cell = (CellView)item.DataContext;
                item.Arrange(new Rect(new Point(_eventProc.CellWidth * (cell.Col - 1), _eventProc.CellHeight * (cell.Row - 1)), new Size(_eventProc.CellWidth, _eventProc.CellHeight)));
            }
            return finalSize;
        }

        #endregion Methods

        #region Internal classes

        private class EventProcImpl : EventProc
        {
            public BoardPanel Panel { get; set; }

            public override double Width { get { return Panel.ActualWidth; } }

            public override double Height { get { return Panel.ActualHeight; } }
            
            public override void CapturePointer(object obj)
            {
                Panel.CapturePointer((Pointer)obj);
            }

            public override void ReleasePointer(object obj)
            {
                Panel.ReleasePointerCapture((Pointer)obj);
            }

            public override void Set(int row, int col)
            {
                Panel._vm.Set(row, col);
            }

            public override void SetOffset(double dx, double dy)
            {
                Panel._tTransform.X = dx;
                Panel._tTransform.Y = dy;
                if (Panel._vm != null) Panel._vm.SetOffset(new Point(dx, dy));
            }
        }

        #endregion Internal classes
    }
}
