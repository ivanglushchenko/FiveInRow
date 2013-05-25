using FiveInRow.UI.Metro.Components;
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using Windows.Foundation;
using Windows.Foundation.Collections;
using Windows.UI.Xaml;
using Windows.UI.Xaml.Controls;
using Windows.UI.Xaml.Controls.Primitives;
using Windows.UI.Xaml.Data;
using Windows.UI.Xaml.Input;
using Windows.UI.Xaml.Media;
using Windows.UI.Xaml.Navigation;

// The Blank Page item template is documented at http://go.microsoft.com/fwlink/?LinkId=234238

namespace FiveInRow.UI.Metro
{
    /// <summary>
    /// An empty page that can be used on its own or navigated to within a Frame.
    /// </summary>
    public sealed partial class MainPage : Page
    {
        #region .ctors

        public MainPage()
        {
            this.InitializeComponent();
        }

        #endregion .ctors

        #region Methods

        protected override void OnNavigatedTo(NavigationEventArgs e)
        {
            DataContext = new MainPageViewModel((GameStartingParams)e.Parameter);
        }

        private void OnGoBack(object sender, RoutedEventArgs e)
        {
            if (DataContext is MainPageViewModel) ((MainPageViewModel)DataContext).GoToMainMenu();
        }

        private void OnRestart(object sender, RoutedEventArgs e)
        {
            ab.IsOpen = false;
            if (DataContext is MainPageViewModel) ((MainPageViewModel)DataContext).Restart();
        }

        private void OnUndo(object sender, RoutedEventArgs e)
        {
            if (DataContext is MainPageViewModel) ((MainPageViewModel)DataContext).Undo();
        }

        private void OnPersistMoves(object sender, RoutedEventArgs e)
        {
            if (DataContext is MainPageViewModel) ((MainPageViewModel)DataContext).PersistMoves();
        }

        #endregion Methods

        private void OnCellLoaded(object sender, RoutedEventArgs e)
        {

        }
    }
}
