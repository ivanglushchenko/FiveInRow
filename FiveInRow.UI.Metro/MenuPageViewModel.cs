using FiveInRow.Foundation;
using FiveInRow.UI.Metro.Components;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Windows.UI;

namespace FiveInRow.UI.Metro
{
    public partial class MenuPageViewModel : ObservableObject
    {
        #region .ctors

        public MenuPageViewModel()
        {
            OpponentAI = true;
            BoardSize19 = true;
        }

        #endregion .ctors

        #region Properties

        public bool OpponentAI { get; set; }
        public bool OpponentAI_Player2 { get; set; }
        public bool OpponentHuman { get; set; }

        public bool BoardSize19 { get; set; }
        public bool BoardSize35 { get; set; }
        public bool BoardSize51 { get; set; }

        #endregion Properties

        #region Methods

        public GameStartingParams ToGameParams()
        {
            return new GameStartingParams()
            {
                BoardSize = BoardSize19 ? 19 : (BoardSize35 ? 35 : 51),
                AILevel = OpponentAI ? AILevel.Easy_P2 : (OpponentAI_Player2 ? AILevel.Easy_P1 : AILevel.Human)
            };
        }

        #endregion Methods
    }
}
