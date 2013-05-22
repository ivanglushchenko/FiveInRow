using FiveInRow.Foundation;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Windows.Foundation;
using Windows.UI.Xaml;
using Windows.UI.Xaml.Controls;
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
        }

        #endregion .ctors

        #region Fields

        private double _cellWidth;
        private double _cellHeigth;
        private bool _isPositioned;

        private bool _isCaptured;
        private bool _isDragging;
        private Point _lastPos;
        private Point _offset;
        private TranslateTransform _tTransform = new TranslateTransform();
        private ItemsControl _owner;
        private MainPageViewModel _vm;

        #endregion Fields

        #region Methods

        void BoardPanel_Loaded(object sender, Windows.UI.Xaml.RoutedEventArgs e)
        {
            Loaded -= BoardPanel_Loaded;

            _owner = ItemsControl.GetItemsOwner(this);
            _vm = (MainPageViewModel)_owner.DataContext;

            _owner.PointerPressed += owner_PointerPressed;
            _owner.PointerReleased += owner_PointerReleased;
            _owner.PointerMoved += owner_PointerMoved;
        }

        void owner_PointerPressed(object sender, Windows.UI.Xaml.Input.PointerRoutedEventArgs e)
        {
            _isCaptured = true;
            _lastPos = e.GetCurrentPoint(_owner).Position;
        }

        void owner_PointerReleased(object sender, Windows.UI.Xaml.Input.PointerRoutedEventArgs e)
        {
            _isCaptured = false;

            if (_isDragging)_isDragging = false;
            else
            {
                var p = e.GetCurrentPoint(_owner).Position;
                var index = new Tuple<int, int>(
                    (int)((p.X - _offset.X) / _cellWidth) + 1,
                    (int)((p.Y - _offset.Y) / _cellHeigth) + 1);
                _vm.Set(index);
            }
        }

        void owner_PointerMoved(object sender, Windows.UI.Xaml.Input.PointerRoutedEventArgs e)
        {
            if (_isCaptured)
            {
                var p = e.GetCurrentPoint(_owner).Position;
                var dx = p.X - _lastPos.X;
                var dy = p.Y - _lastPos.Y;
                if (_isDragging || (dx * dx + dy * dy > 36))
                {
                    _isDragging = true;
                    AddOffset(dx, dy);
                    _lastPos = p;
                }
            }
        }

        private void AddOffset(double dx, double dy)
        {
            _offset.X += dx;
            _offset.Y += dy;
            _tTransform.X = _offset.X;
            _tTransform.Y = _offset.Y;
        }

        protected override Size MeasureOverride(Size availableSize)
        {
            if (Children.Count == 0)
                return base.MeasureOverride(availableSize);
            else
            {
                foreach (var item in Children)
                {
                    item.Measure(availableSize);
                }
                _cellWidth = Children.Select(t => t.DesiredSize.Width).Max();
                _cellHeigth = Children.Select(t => t.DesiredSize.Height).Max();

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
                    var maxI = Children.OfType<FrameworkElement>().Select(c => c.DataContext).OfType<Cell>().Select(c => c.X).Max();
                    var maxJ = Children.OfType<FrameworkElement>().Select(c => c.DataContext).OfType<Cell>().Select(c => c.Y).Max();

                    var dx = (finalSize.Width - maxI * _cellWidth) / 2.0;
                    var dy = (finalSize.Height - maxJ * _cellHeigth) / 2.0;
                    AddOffset(dx, dy);
                }
            }

            foreach (var item in Children.OfType<FrameworkElement>())
            {
                var cell = (Cell)item.DataContext;
                item.Arrange(new Rect(new Point(_cellWidth * (cell.X - 1), _cellHeigth * (cell.Y - 1)), new Size(_cellWidth, _cellHeigth)));
            }
            return finalSize;
        }

        #endregion Methods
    }
}
