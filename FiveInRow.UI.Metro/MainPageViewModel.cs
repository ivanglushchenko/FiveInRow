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
            Board = Board.CreateNew(gameParams.BoardSize, gameParams.BoardSize);
            Board.ControlsXPlayer = true;
            Board.ControlsOPlayer = gameParams.AILevel == 0;
        }

        #endregion .ctors

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

        public async void Set(Tuple<int, int> index)
        {
            if (IsCompleted) return;

            var result = Board.Set(index);
            if (result != null)
            {
                IsCompleted = true;

                var dialog = new Windows.UI.Popups.MessageDialog(string.Format("Player {0} won the game", result.Value == CellValue.X ? "1" : "2"));
                dialog.Commands.Add(new UICommand("Start new game", new UICommandInvokedHandler((cmd) => 
                {
                    IsCompleted = false;
                    Board = Board.CreateNew(51, 51);
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
