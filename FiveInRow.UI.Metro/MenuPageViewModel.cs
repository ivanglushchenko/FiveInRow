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

        public bool OpponentHuman
        {
            get { return p_OpponentHuman; }
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
        public bool DiffMedium { get; set; }
        public bool DiffHard { get; set; }

        #endregion Properties

        #region Methods

        public GameSettings ToGameParams()
        {
            return new GameSettings()
            {
                BoardSize = BoardSize19 ? 19 : (BoardSize35 ? 35 : 51),
                Opponent = OpponentAI ? GameDef.OpponentType.NewAI(GameDef.Player.Player2) : (OpponentAI_Player2 ? GameDef.OpponentType.NewAI(GameDef.Player.Player1) : GameDef.OpponentType.Human),
                Difficulty = DiffEasy ? GameDef.Difficulty.Easy : (DiffMedium ? GameDef.Difficulty.Medium : GameDef.Difficulty.Hard)
            };
        }

        #endregion Methods
    }
}
