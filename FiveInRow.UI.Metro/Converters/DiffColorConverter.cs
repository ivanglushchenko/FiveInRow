using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Windows.UI;
using Windows.UI.Xaml.Data;
using Windows.UI.Xaml.Media;

namespace FiveInRow.UI.Metro.Converters
{
    public class DiffColorConverter : IValueConverter
    {
        public DiffColorConverter()
        {
            _disabled = new SolidColorBrush(Color.FromArgb(40, 255, 255, 255));
            _enabled = new SolidColorBrush(Colors.White);
        }

        private Brush _disabled, _enabled;

        public object Convert(object value, Type targetType, object parameter, string language)
        {
            return (bool)value ? _disabled : _enabled;
        }

        public object ConvertBack(object value, Type targetType, object parameter, string language)
        {
            throw new NotImplementedException();
        }
    }
}
