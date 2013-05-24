using FiveInRow.Foundation;
using FiveInRow.UI.Metro.Components;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Windows.Foundation;
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
            Start();
        }

        #endregion .ctors

        #region Fields

        private GameStartingParams _params;
        private Point _offset;
        private BoardPanel _panel;

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
        /// Gets/sets WinningRow.
        /// </summary>
        public object WinningRow
        {
            [System.Diagnostics.DebuggerStepThrough]
            get { return p_WinningRow; }
            [System.Diagnostics.DebuggerStepThrough]
            set
            {
                if (p_WinningRow != value)
                {
                    p_WinningRow = value;
                    OnPropertyChanged("WinningRow");
                    OnWinningRowChanged();
                }
            }
        }
        private object p_WinningRow;
        partial void OnWinningRowChanged();

        #endregion Properties

        #region Methods

        public void SetPanel(BoardPanel panel)
        {
            _panel = panel;
        }

        public async void Set(int row, int col)
        {
            if (Board.Winner != null) return;

            Board.Set(row, col);
            if (Board.Winner == null && _params.AILevel != AILevel.Human) Board.MakeAIMove();
            if (Board.Winner != null)
            {
                RefreshWinningRow();

                var dialog = new Windows.UI.Popups.MessageDialog(string.Format("Player {0} won the game, golf clap for you!", Board.Winner.Value == GameDef.Player.Player1 ? "1" : "2"));
                dialog.Commands.Add(new UICommand("Start new game", new UICommandInvokedHandler((cmd) => Restart())));
                dialog.Commands.Add(new UICommand("Return to main menu", new UICommandInvokedHandler((cmd) => GoToMainMenu())));
                dialog.Commands.Add(new UICommand("Give me a break", new UICommandInvokedHandler((cmd) => { })));
                await dialog.ShowAsync();
            }
        }

        public void Restart()
        {
            Board.Clear();
            WinningRow = null;
            if (_panel != null) _panel.Centrify();
            Start();
        }

        public void GoToMainMenu()
        {
            var rootFrame = Window.Current.Content as Frame;
            if (rootFrame != null)
            {
                rootFrame.Navigate(typeof(MenuPage));
            }
        }

        public void SetOffset(Point offset)
        {
            _offset = offset;
            RefreshWinningRow();
        }

        public void Undo()
        {
            if (Board.Winner != null) return;

            switch (_params.AILevel)
            {
                case AILevel.Easy_P1:
                    if (Board.Moves.Length >= 3)
                    {
                        Board.Undo();
                        Board.Undo();
                    }
                    break;
                case AILevel.Easy_P2:
                    Board.Undo();
                    Board.Undo();
                    break;
                case AILevel.Human:
                    Board.Undo();
                    break;
            }
        }

        private void RefreshWinningRow()
        {
            if (Board.Winner == null)
            {
                WinningRow = null;
            }
            else
            {
                var row = Board.FiveInRows.First();
                var rnd = new Random();
                WinningRow = new
                {
                    X1 = (row.From.Item2 - 0.5) * 60.0 + _offset.X,
                    Y1 = (row.From.Item1 - 0.5) * 60.0 + _offset.Y,
                    X2 = (row.To.Item2 - 0.5) * 60.0 + _offset.X,
                    Y2 = (row.To.Item1 - 0.5) * 60.0 + _offset.Y
                };
            }
        }

        private void Start()
        {
            if (_params.AILevel == AILevel.Easy_P1)
            {
                Board.MakeAIMove();
            }
        }

        #endregion Methods
    }
}
