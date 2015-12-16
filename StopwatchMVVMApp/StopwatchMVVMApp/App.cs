using Merlin.Common;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Diagnostics;
using Xamarin.Forms;

namespace StopwatchMVVMApp
{
	public class App : Application
	{
		public App()
		{
			Log.logF = (t, s) => Debug.WriteLine("{0}-{1}", t, s);
			StopwatchMVVMAppModel.Model.onCtxF = Device.BeginInvokeOnMainThread;
			// The root page of your application
			MainPage = new ViewPage();
		}

		protected override void OnStart()
		{
			// Handle when your app starts
		}

		protected override void OnSleep()
		{
			// Handle when your app sleeps
		}

		protected override void OnResume()
		{
			// Handle when your app resumes
		}
	}
}
