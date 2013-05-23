using FiveInRow.Foundation;
using FiveInRow.UI.Metro.Components;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Windows.UI.Popups;
using Windows.UI.Xaml;
using Windows.UI.Xaml.Controls;

namespace FiveInRow.UI.Metro
{
    public partial class MainPageViewModel : ObservableObject
    {
        #region .ctors

        public MainPageViewModel(GameStartingParams gameParams)
        {
            _params = gameParams;
            Board = BoardView.Create(gameParams.BoardSize);
        }

        #endregion .ctors

        #region Fields

        private GameStartingParams _params;

        #endregion Fields

        #region Properties

        /// <summary>
        /// Gets/sets Board.
        /// </summary>
        public BoardView Board
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
        private BoardView p_Board;
        partial void OnBoardChanged();

        /// <summary>
        /// Gets/sets IsCompleted.
        /// </summary>
        public bool IsCompleted
        {
            [System.Diagnostics.DebuggerStepThrough]
            get { return p_IsCompleted; }
            [System.Diagnostics.DebuggerStepThrough]
            set
            {
                if (p_IsCompleted != value)
                {
                    p_IsCompleted = value;
                    OnPropertyChanged("IsCompleted");
                    OnIsCompletedChanged();
                }
            }
        }
        private bool p_IsCompleted;
        partial void OnIsCompletedChanged();

        #endregion Properties

        #region Methods

        public async void Set(int row, int col)
        {
            if (IsCompleted) return;

            Board.Set(row, col);
            if (Board.Winner == null && _params.AILevel != AILevel.Human) Board.MakeAIMove();
            if (Board.Winner != null)
            {
                IsCompleted = true;

                var dialog = new Windows.UI.Popups.MessageDialog(string.Format("Player {0} won the game", Board.Winner.Value == GameDef.Player.Player1 ? "1" : "2"));
                dialog.Commands.Add(new UICommand("Start new game", new UICommandInvokedHandler((cmd) => 
                {
                    IsCompleted = false;
                    Board.Clear();
                })));
                dialog.Commands.Add(new UICommand("Return to main menu", new UICommandInvokedHandler((cmd) => 
                {
                    var rootFrame = Window.Current.Content as Frame;
                    if (rootFrame != null)
                    {
                        rootFrame.Navigate(typeof(MenuPage));
                    }
                })));
                dialog.Commands.Add(new UICommand("Give me a break", new UICommandInvokedHandler((cmd) => { })));
                await dialog.ShowAsync();
            }
        }

        #endregion Methods
    }
}
