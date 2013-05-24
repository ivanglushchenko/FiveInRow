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
            DiffEasy = true;
        }

        #endregion .ctors

        #region Properties

        public bool OpponentAI { get; set; }
        public bool OpponentAI_Player2 { get; set; }

        /// <summary>
        /// Gets/sets OpponentHuman.
        /// </summary>
        public bool OpponentHuman
        {
            [System.Diagnostics.DebuggerStepThrough]
            get { return p_OpponentHuman; }
            [System.Diagnostics.DebuggerStepThrough]
            set
            {
                if (p_OpponentHuman != value)
                {
                    p_OpponentHuman = value;
                    OnPropertyChanged("OpponentHuman");
                }
            }
        }
        private bool p_OpponentHuman;

        public bool BoardSize19 { get; set; }
        public bool BoardSize35 { get; set; }
        public bool BoardSize51 { get; set; }

        public bool DiffEasy { get; set; }
        public bool DiffHard { get; set; }

        #endregion Properties

        #region Methods

        public GameStartingParams ToGameParams()
        {
            return new GameStartingParams()
            {
                BoardSize = BoardSize19 ? 19 : (BoardSize35 ? 35 : 51),
                Opponent = OpponentAI ? OpponentType.Easy_P2 : (OpponentAI_Player2 ? OpponentType.Easy_P1 : OpponentType.Human),
                Difficulty = DiffEasy ? GameDef.Difficulty.Easy : GameDef.Difficulty.Medium
            };
        }

        #endregion Methods
    }
}
