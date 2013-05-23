using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace FiveInRow.UI.Metro.Components
{
    public class GameStartingParams
    {
        #region Properties

        public AILevel AILevel { get; set; }
        public int BoardSize { get; set; }

        #endregion Properties
    }

    public enum AILevel
    {
        Human,
        Easy
    }
}
