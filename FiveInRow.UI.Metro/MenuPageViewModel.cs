using FiveInRow.UI.Metro.Components;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Reflection;
using Windows.UI;
using FiveInRow.Core.UI;

namespace FiveInRow.UI.Metro
{
    public partial class MenuPageViewModel : GameSettingsVM
    {
        #region .ctors

        public MenuPageViewModel()
        {
            LoadSettings();
        }

        #endregion .ctors

        #region Methods

        private async void LoadSettings()
        {
            var gs = await SettingsStore.Get();
            foreach (var item in gs.GetType().GetTypeInfo().DeclaredProperties)
            {
                item.SetValue(this, item.GetValue(gs));
            }
        }

        public GameSettingsVM GetSettings()
        {
            var gs = new GameSettingsVM();
            foreach (var item in gs.GetType().GetTypeInfo().DeclaredProperties)
            {
                item.SetValue(gs, item.GetValue(this));
            }
            return gs;
        }

        #endregion Method
    }
}
