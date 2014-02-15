using FiveInRow.Core.UI;
using FiveInRow.Foundation;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Windows.UI.Xaml;
using Windows.UI.Xaml.Data;

namespace FiveInRow.UI.Metro.Converters
{
    public partial class CellValue2VisibilityConverter : IValueConverter
    {
        public object Convert(object value, Type targetType, object parameter, string language)
        {
            if (parameter == null)
            {
                return value == null ? Visibility.Visible : Visibility.Collapsed;
            }
            if (value is CellValue.Occupied)
            {
                var occupied = value as CellValue.Occupied;
                return
                    ((occupied.Item == FiveInRow.Core.GameDef.Player.Player1 && (string)parameter == "X") || (occupied.Item == FiveInRow.Core.GameDef.Player.Player2 && (string)parameter == "O"))
                    ? Visibility.Visible
                    : Visibility.Collapsed;
            }
            return Visibility.Collapsed;
        }

        public object ConvertBack(object value, Type targetType, object parameter, string language)
        {
            throw new NotImplementedException();
        }
    }
}
