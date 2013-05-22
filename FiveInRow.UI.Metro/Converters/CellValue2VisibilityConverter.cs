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

            return
                ((value == CellValue.X && (string)parameter == "X") || (value == CellValue.O && (string)parameter == "O")) 
                ? Visibility.Visible 
                : Visibility.Collapsed;
        }

        public object ConvertBack(object value, Type targetType, object parameter, string language)
        {
            throw new NotImplementedException();
        }
    }
}
