namespace StopwatchMVVMAppModel
open System.ComponentModel
open System.Collections.ObjectModel
open Merlin.Common
open System.Threading
open System.Diagnostics
open System
module Model=
  //util 
  //SynchronizationContextでうまくいかなかったので注入で…なんで？
  let mutable onCtxF=null:Action<Action>
  let onCtx f= (onCtxF).Invoke(fun()-> f())
//  let ctx=new SynchronizationContext()
//  let onCtx f=ctx.Post((fun _->f()),null)
  let spawnTimer interval f=
    let cts=new CancellationTokenSource()
    let rec loop()=
      async{
        onCtx f
        do! Async.Sleep interval
        return! loop()
      }
    Async.Start(loop(),cts.Token)
    {new IDisposable with member x.Dispose()=cts.Cancel()}      
  //modelへのリクエスト
  type Msg=
    |MsgStart_Stop
    |MsgLap
  //Lapの値
  type LapVal=
    { No:int;Time:int64}
  let lapTime v=v.Time
  //計測結果
  type LapResult=
    { Max:double
      Min:double
      Avg:double
    }
  let toResult times=
    { Max=Seq.max times
      Min=Seq.min times
      Avg=Seq.average times}
  //modelの状態
  type MState< ^a>=
    |MSNone
    |MSStarted
    |MSStopped of ^a
  let newModel()=
    //modelの値
    let tSub=sub 0L
    let statSub=sub MSNone
    let lObCol=ObservableCollection<_>()
    let toLapV t={No=lObCol.Count+1;Time=t}
    //状態遷移
    let actor=Actor<_>.Start(fun mbox->
      //start状態
      let rec startLoop (sw:Stopwatch) lastEMSec=
        lObCol.Clear()//前の残りあれば消す
        let timerD=spawnTimer 3<|fun ()->tSub.Value<-sw.ElapsedMilliseconds
        let rec loop lastEMSec=
          async{
            let! msg=mbox.Receive()
            match msg with
            |MsgStart_Stop->
              //stopの処理
              timerD.Dispose()
              let eMSec=sw.ElapsedMilliseconds
              onCtx<|fun()->
                lObCol.Add<|toLapV(eMSec-lastEMSec)
                let r=lObCol|>Seq.map(lapTime>>double)|>toResult
                statSub.Value<-MSStopped r
              //stop状態へ
              return! stopLoop()
            |MsgLap->
              //Lap追加してstart状態続行
              let eMSec=sw.ElapsedMilliseconds
              onCtx<|fun()->lObCol.Add<|toLapV(eMSec-lastEMSec)
              return! loop eMSec
          }
        loop lastEMSec
      //stop状態
      and stopLoop()=
        async{
          let! msg=mbox.Receive()
          match msg with
          |MsgStart_Stop->
            //start状態へ
            onCtx<|fun()->statSub.Value<-MSStarted
            let sw=Stopwatch.StartNew()
            return! startLoop sw 0L
          |v->
            Log.logErr<|sprintf"unknown msg=%A" v
            return! stopLoop()
        }
      stopLoop()
    )
    actor,tSub,statSub,lObCol     
open Model
open VmUtil
module ViewModel=
  type SWatchVM()=
    //model
    let actor,tSub,statSub,lObCol=newModel()
    //ui related
    let vEvt=Event<_>()//messengerがわり
    let startStopCmd=toEverCmd<|fun _->actor.Post MsgStart_Stop
    let lapCmd,lapCmdCanE=toCmd (fun _->statSub.Value=MSStarted)
                                (fun _->actor.Post MsgLap)
    (*stateによりlapボタンの状態変更。
      StoppedだったらResult表示*)
    let _=statSub.AsObservable()
          |>Observable.subscribe(fun stat->
            lapCmdCanE()
            match stat with
            |MSStopped v->vEvt.Trigger v
            |_->()
            )
    //stateによりボタンの表示変更
    let btnSSub,_=statSub.AsObservable()
                  |>Observable.map (function
                    |MSNone->"Start"
                    |MSStarted->"Stop"
                    |MSStopped _->"Restart")
                  |>strongObToSub
    member x.BtnSSub=btnSSub
    member x.LapCmd=lapCmd
    member x.StartStopCmd=startStopCmd
    member x.TSub=tSub
    member x.LObCol=lObCol
    [<CLIEvent>]
    member x.ViewEvt=vEvt.Publish
