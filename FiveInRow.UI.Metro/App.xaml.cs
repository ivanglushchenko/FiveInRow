using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using Windows.ApplicationModel;
using Windows.ApplicationModel.Activation;
using Windows.Foundation;
using Windows.Foundation.Collections;
using Windows.UI.ApplicationSettings;
using Windows.UI.Xaml;
using Windows.UI.Xaml.Controls;
using Windows.UI.Xaml.Controls.Primitives;
using Windows.UI.Xaml.Data;
using Windows.UI.Xaml.Input;
using Windows.UI.Xaml.Media;
using Windows.UI.Xaml.Media.Animation;
using Windows.UI.Xaml.Navigation;

// The Blank Application template is documented at http://go.microsoft.com/fwlink/?LinkId=234227

namespace FiveInRow.UI.Metro
{
    /// <summary>
    /// Provides application-specific behavior to supplement the default Application class.
    /// </summary>
    sealed partial class App : Application
    {
        #region .ctors

        public App()
        {
            this.InitializeComponent();
            this.Suspending += OnSuspending;
        }

        #endregion .ctors

        #region Methods

        /// <summary>
        /// Invoked when the application is launched normally by the end user.  Other entry points
        /// will be used when the application is launched to open a specific file, to display
        /// search results, and so forth.
        /// </summary>
        /// <param name="args">Details about the launch request and process.</param>
        protected override void OnLaunched(LaunchActivatedEventArgs args)
        {
            Frame rootFrame = Window.Current.Content as Frame;

            // Do not repeat app initialization when the Window already has content,
            // just ensure that the window is active
            if (rootFrame == null)
            {
                // Create a Frame to act as the navigation context and navigate to the first page
                rootFrame = new Frame();

                if (args.PreviousExecutionState == ApplicationExecutionState.Terminated)
                {
                    //TODO: Load state from previously suspended application
                }

                // Place the frame in the current Window
                Window.Current.Content = rootFrame;
            }

            if (rootFrame.Content == null)
            {
                // When the navigation stack isn't restored navigate to the first page,
                // configuring the new page by passing required information as a navigation
                // parameter
                if (!rootFrame.Navigate(typeof(MenuPage), args.Arguments))
                {
                    throw new Exception("Failed to create initial page");
                }
            }
            // Ensure the current window is active
            Window.Current.Activate();

            SettingsPane.GetForCurrentView().CommandsRequested += App_CommandsRequested;
        }

        /// <summary>
        /// Invoked when application execution is being suspended.  Application state is saved
        /// without knowing whether the application will be terminated or resumed with the contents
        /// of memory still intact.
        /// </summary>
        /// <param name="sender">The source of the suspend request.</param>
        /// <param name="e">Details about the suspend request.</param>
        private void OnSuspending(object sender, SuspendingEventArgs e)
        {
            var deferral = e.SuspendingOperation.GetDeferral();
            //TODO: Save application state and stop any background activity
            deferral.Complete();
        }

        private Popup _settingsPopup;

        private void App_CommandsRequested(SettingsPane sender, SettingsPaneCommandsRequestedEventArgs args)
        {
            args.Request.ApplicationCommands.Add(new SettingsCommand("Privacy policy", "Privacy policy", _ =>
                {
                    _settingsPopup = new Popup();
                    _settingsPopup.Closed += OnPopupClosed;
                    Window.Current.Activated += OnWindowActivated;
                    _settingsPopup.IsLightDismissEnabled = true;
                    _settingsPopup.Width = 346;
                    _settingsPopup.Height = Window.Current.Bounds.Height;

                    // Add the proper animation for the panel.
                    _settingsPopup.ChildTransitions = new TransitionCollection();
                    _settingsPopup.ChildTransitions.Add(new PaneThemeTransition()
                    {
                        Edge = (SettingsPane.Edge == SettingsEdgeLocation.Right) ?
                               EdgeTransitionLocation.Right :
                               EdgeTransitionLocation.Left
                    });

                    // Create a SettingsFlyout the same dimenssions as the Popup.
                    var mypane = new PrivacyPolicyPage();
                    mypane.Width = 346;
                    mypane.Height = Window.Current.Bounds.Height;

                    // Place the SettingsFlyout inside our Popup window.
                    _settingsPopup.Child = mypane;

                    // Let's define the location of our Popup.
                    _settingsPopup.SetValue(Canvas.LeftProperty, SettingsPane.Edge == SettingsEdgeLocation.Right ? (Window.Current.Bounds.Width - 346) : 0);
                    _settingsPopup.SetValue(Canvas.TopProperty, 0);
                    _settingsPopup.IsOpen = true;

                }));
        }

        private void OnWindowActivated(object sender, Windows.UI.Core.WindowActivatedEventArgs e)
        {
            if (e.WindowActivationState == Windows.UI.Core.CoreWindowActivationState.Deactivated)
            {
                _settingsPopup.IsOpen = false;
            }
        }

        void OnPopupClosed(object sender, object e)
        {
            var popup = sender as Popup;
            if (popup.Child is FrameworkElement && (popup.Child as FrameworkElement).DataContext is IDisposable)
                ((popup.Child as FrameworkElement).DataContext as IDisposable).Dispose();

            Window.Current.Activated -= OnWindowActivated;
        }
        #endregion Methods
    }
}
