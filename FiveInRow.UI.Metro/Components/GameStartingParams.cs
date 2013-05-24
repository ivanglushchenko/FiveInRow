using FiveInRow.Foundation;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace FiveInRow.UI.Metro.Components
{
    public class GameStartingParams : GameSettings
    {
        #region Properties

        public OpponentType Opponent { get; set; }

        #endregion Properties
    }

    public enum OpponentType
    {
        Human,
        Easy_P2,
        Easy_P1
    }
}
