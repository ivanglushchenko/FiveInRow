using FiveInRow.Foundation;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace FiveInRow.UI.Metro
{
    public partial class MainPageViewModel : ObservableObject
    {
        #region .ctors

        public MainPageViewModel()
        {
            Board = Board.CreateNew(19, 19);
        }

        #endregion .ctors

        #region Fields


        #endregion Fields

        #region Properties

        /// <summary>
        /// Gets/sets Board.
        /// </summary>
        public Board Board
        {
            [System.Diagnostics.DebuggerStepThrough]
            get { return p_Board; }
            [System.Diagnostics.DebuggerStepThrough]
            set
            {
                if (p_Board != value)
                {
                    p_Board = value;
                    OnPropertyChanged("Board");
                    OnBoardChanged();
                }
            }
        }
        private Board p_Board;
        partial void OnBoardChanged();

        #endregion Properties

        #region Methods

        public void Set(Tuple<int, int> index)
        {
            Board.Set(index);
        }

        #endregion Methods
    }
}
