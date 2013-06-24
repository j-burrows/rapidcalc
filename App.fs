module MainApp

open System
open System.Windows
open System.Windows.Controls
open System.Windows.Markup
open RapidCalc.ViewModels

// Create the View and bind it to the View Model
let homeViewModel = Application.LoadComponent(
                             new System.Uri("/App;component/mainwindow.xaml", UriKind.Relative)) :?> Window
homeViewModel.DataContext <- new HomeViewModel() 

// Application Entry point
[<STAThread>]
[<EntryPoint>]
let main(_) = 
    (new Application()).Run(homeViewModel);