using FiveInRow.Core.UI;
using FiveInRow.Foundation;
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Runtime.Serialization.Json;
using System.Text;
using System.Threading.Tasks;
using System.Xml.Serialization;
using Windows.Storage;
using Windows.Storage.Search;

namespace FiveInRow.UI.Metro.Components
{
    public class SettingsStore
    {
        #region Properties

        public static GameSettingsVM DefaultSettings
        {
            get
            {
                return new GameSettingsVM() { BoardSize19 = true, DiffEasy = true, OpponentAIPlayer2 = true };
            }
        }

        #endregion Properties

        #region Methods

        public static async Task<GameSettingsVM> Get()
        {
            try
            {
                var files = await ApplicationData.Current.LocalFolder.GetFilesAsync(CommonFileQuery.DefaultQuery);
                var file = files.FirstOrDefault(f => f.Name == "lastSettings");
                if (file == null)
                    return DefaultSettings;

                var str = await FileIO.ReadTextAsync(file, Windows.Storage.Streams.UnicodeEncoding.Utf8); ;
                if (str == null)
                    return DefaultSettings;
                else
                    return Deserialize<GameSettingsVM>(str) ?? DefaultSettings;
            }
            catch
            {
            }
            return DefaultSettings;
        }

        public static async void Put(GameSettingsVM s)
        {
            try
            {
                var files = await ApplicationData.Current.LocalFolder.GetFilesAsync(CommonFileQuery.DefaultQuery);
                var file = files.FirstOrDefault(f => f.Name == "lastSettings");
                if (file == null)
                    file = await ApplicationData.Current.LocalFolder.CreateFileAsync("lastSettings");
                await FileIO.WriteTextAsync(file, Serialize(s));
            }
            catch
            {
            }
        }

        private static T Deserialize<T>(string json)
        {
            if (string.IsNullOrEmpty(json)) return default(T);

            var _Bytes = Encoding.Unicode.GetBytes(json);
            using (var _Stream = new MemoryStream(_Bytes))
            {
                var _Serializer = new XmlSerializer(typeof(T));
                return (T)_Serializer.Deserialize(_Stream);
            }
        }

        private static string Serialize(object instance)
        {
            using (var _Stream = new MemoryStream())
            {
                var _Serializer = new XmlSerializer(instance.GetType());
                _Serializer.Serialize(_Stream, instance);
                _Stream.Position = 0;
                using (StreamReader _Reader = new StreamReader(_Stream))
                { return _Reader.ReadToEnd(); }
            }
        }

        #endregion Methods
    }
}
