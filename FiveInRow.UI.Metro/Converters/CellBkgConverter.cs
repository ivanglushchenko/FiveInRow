using FiveInRow.Core;
using FiveInRow.Core.UI;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Windows.Foundation;
using Windows.UI;
using Windows.UI.Xaml.Data;
using Windows.UI.Xaml.Media;

namespace FiveInRow.UI.Metro.Converters
{
    public class CellBkgConverter : IValueConverter
    {
        #region .ctors

        public CellBkgConverter()
        {
            _normal = new LinearGradientBrush() { StartPoint = new Point(0, 0), EndPoint = new Point(1, 1) };
            _normal.GradientStops.Add(new GradientStop() { Color = Colors.Gray, Offset = 0 });
            _normal.GradientStops.Add(new GradientStop() { Color = Colors.Gray, Offset = 0.5 });
            _normal.GradientStops.Add(new GradientStop() { Color = Colors.Transparent, Offset = 0.5 });
            _normal.GradientStops.Add(new GradientStop() { Color = Colors.Transparent, Offset = 1 });

            _rightBorder = new LinearGradientBrush() { StartPoint = new Point(0, 0), EndPoint = new Point(0, 1) };
            _rightBorder.GradientStops.Add(new GradientStop() { Color = Colors.Gray, Offset = 0 });
            _rightBorder.GradientStops.Add(new GradientStop() { Color = Colors.Gray, Offset = 0.95 });
            _rightBorder.GradientStops.Add(new GradientStop() { Color = Colors.Transparent, Offset = 0.95 });
            _rightBorder.GradientStops.Add(new GradientStop() { Color = Colors.Transparent, Offset = 1 });

            _bottomBorder = new LinearGradientBrush() { StartPoint = new Point(0, 0), EndPoint = new Point(1, 0) };
            _bottomBorder.GradientStops.Add(new GradientStop() { Color = Colors.Gray, Offset = 0 });
            _bottomBorder.GradientStops.Add(new GradientStop() { Color = Colors.Gray, Offset = 0.95 });
            _bottomBorder.GradientStops.Add(new GradientStop() { Color = Colors.Transparent, Offset = 0.95 });
            _bottomBorder.GradientStops.Add(new GradientStop() { Color = Colors.Transparent, Offset = 1 });

            _corner = new SolidColorBrush(Colors.Gray);
        }

        #endregion .ctors

        #region Fields

        private LinearGradientBrush _normal;
        private LinearGradientBrush _rightBorder;
        private LinearGradientBrush _bottomBorder;
        private Brush _corner;

        #endregion Fields

        #region Methods

        public object Convert(object value, Type targetType, object parameter, string language)
        {
            var cell = (CellView)value;
            if (cell.Col == GameDef.boardDimension)
            {
                return cell.Row == GameDef.boardDimension ? _corner : _rightBorder;
            }
            else if (cell.Row == GameDef.boardDimension) return _bottomBorder;
            return _normal;
        }

        public object ConvertBack(object value, Type targetType, object parameter, string language)
        {
            throw new NotImplementedException();
        }

        #endregion Methods
    }
}
