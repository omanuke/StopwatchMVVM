namespace Merlin.Common
open System
open System.ComponentModel
open System.Windows.Input

[<AutoOpen>]
module VmUtil=
  let toCmd canExecute action=
      let canExecuteChanged=Event<_,_>()
      let cmd=
          {new ICommand with
              [<CLIEvent>]
              member this.CanExecuteChanged=canExecuteChanged.Publish
              member this.CanExecute param=canExecute param
              member this.Execute param=action param}
      cmd,fun ()->canExecuteChanged.Trigger(cmd,EventArgs.Empty)
  let toEverCmd action=toCmd (fun p->true) action|>fst
  
  type BindableBase() as this=
    let propChanged=Event<_,_>()
    let raisePropChanged propName=
      propChanged.Trigger(this,PropertyChangedEventArgs(propName))
    let setProp store propName value=
        if !store=value then false
        else
            store:=value
            propChanged.Trigger(this,PropertyChangedEventArgs(propName))
            true
    let toProp initVal name=
      let rVal=ref initVal
      rVal,setProp rVal name
    interface INotifyPropertyChanged with
        [<CLIEvent>]
        member this.PropertyChanged=propChanged.Publish
    member this.ToProp<'a when 'a:equality> (v:'a)=toProp v
    member this.RaisePropertyChanged propName=raisePropChanged propName
      
