using System;
using System.Collections.Generic;
using System.Globalization;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

using Xamarin.Forms;
using static StopwatchMVVMAppModel.ViewModel;

namespace StopwatchMVVMApp
{
	public partial class ViewPage : ContentPage
	{
		public ViewPage()
		{
			InitializeComponent();
			var vm = new SWatchVM();
			vm.ViewEvt += async (_, r) => {
				var s = string.Format("Max={0}\r\nMin={1}\r\nAvg={2}", r.Max, r.Min, r.Avg);
				await DisplayAlert("Result",s,"OK");
  				};
			BindingContext = vm;
		}
	}

	class TToTimeSVConverter : IValueConverter
	{
		public object Convert(object value, Type targetType, object parameter, CultureInfo culture)
		{
			var v = (Int64)value;
			var m = v / 1000 / 60;
			var s = v / 1000 % 60;
			var msec = v % 1000;
			return string.Format("{0:#00}'{1:00}\"{2:000}", m, s, msec);
		}

		public object ConvertBack(object value, Type targetType, object parameter, CultureInfo culture)
		{
			throw new NotImplementedException();
		}
	}
}
