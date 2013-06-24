(*
 +-- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
 |  Filename:   ViewModelBase.fs
 +-- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
 |  Purpose:    Provide a general class which implements INotifyProperty changed which will
 |              used by view models
 +-- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
 |  Author:     Jonathan Burrows
 |  Date:       May 31st, 2013
 +-- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
*)
namespace RapidCalc.ViewModels

open System
open System.Windows
open System.Windows.Input
open System.ComponentModel

type ViewModelBase() =
    let propertyChangedEvent = new DelegateEvent<PropertyChangedEventHandler>()
    interface INotifyPropertyChanged with
        [<CLIEvent>]
        member x.PropertyChanged = propertyChangedEvent.Publish
    member x.OnPropertyChanged propertyName = 
        propertyChangedEvent.Trigger([| x; new PropertyChangedEventArgs(propertyName) |])    