namespace Merlin.Common
open System
open System.Diagnostics
open System.Runtime.ExceptionServices
open System.ComponentModel
module Log=
  type LogType=
    |Debug=0
    |Info=1
    |Warn=2
    |Error=3
    |Fatal=4
  let mutable logF=null:Action<LogType,string>
  let log s=logF.Invoke(LogType.Info,s)
  let logWarn s=logF.Invoke(LogType.Warn,s)
  let logErr s=logF.Invoke(LogType.Error,s)
  let logFatal s=logF.Invoke(LogType.Fatal,s)
  let logDebug s=logF.Invoke(LogType.Debug,s)

[<AutoOpen>]
module Util=
  open System.Collections.Generic
  open System.Threading

  let raiseArgEx s=raise(System.ArgumentException(s))

  type Instance<'a>(v:'a)=
    member this.Value=v
  type UniqueId()=
    static member New()=UniqueId()

  type Actor<'a>=MailboxProcessor<'a>
  //Msgを使うよう。外部にさらすインターフェースを限定。
  type IActor<'Msg>=
    abstract Req: 'Msg->unit
    abstract ReqRep:(AsyncReplyChannel<'Rep>->'Msg)->Async<'Rep>
  let startActor f=
    let actor=Actor.Start f
    {new IActor<_> with
      member this.Req req=actor.Post req
      member this.ReqRep build=
        actor.PostAndAsyncReply build
      }
  type ActorWrap()=
    let cts=new CancellationTokenSource()
    let actor=Actor.Start((fun actor->
      let rec loop()=async{
        let! asyncF=actor.Receive()
        try do! asyncF
        with ex->Log.logErr<|ex.ToString()
        return! loop()
      }
      loop()),cts.Token)
    do actor.Error|>Event.add(fun ex->Log.logErr<|sprintf "actor err raised.err=%s" (ex.ToString()))
    let postReq f=actor.Post <|async{f()}
    let postAsyncReq asyncF=actor.Post <|asyncF
    let reqReply f timeout=
      actor.PostAndAsyncReply((fun rep->
        async{let v=f()
              rep.Reply v}),timeout)
    let reqAsyncReply asyncF timeout=
      actor.PostAndAsyncReply((fun rep->
        async{
          let! v=asyncF
          rep.Reply v}),timeout)
    interface IDisposable with
      member this.Dispose()=cts.Cancel()
    member this.PostReq f=postReq f
    member this.PostAsyncReq asyncF=postAsyncReq asyncF
    member this.ReqReply(f,?timeout)=
      let timeout=defaultArg timeout -1
      reqReply f timeout
    member this.ReqAsyncReply(asyncF,?timeout)=
      let timeout=defaultArg timeout -1
      reqAsyncReply asyncF timeout
    member this.ErrorEvt=actor.Error
  (*http://stackoverflow.com/questions/18192328/how-to-get-a-useful-stacktrace-when-testing-f-async-workflows
    RunSynchronouslyでまともなスタックとるため*)
  type Async with
    static member Rethrow x =
      match x with 
        | Choice1Of2 x -> x
        | Choice2Of2 ex -> ExceptionDispatchInfo.Capture(ex).Throw()
                           failwith "nothing to return, but will never get here"

  let toKV (k,v)=KeyValuePair<_,_>(k,v)
  let kvKey (kv:KeyValuePair<_,_>)=kv.Key
  let kvVal (kv:KeyValuePair<_,_>)=kv.Value
  let kvTpl (kv:KeyValuePair<_,_>)=kv.Key,kv.Value

  let dispose (d : IDisposable) = d.Dispose()
  let toDisposable f={new IDisposable with member this.Dispose()=f()}
  let disposeOpt (dOpt:IDisposable option)=dOpt|>Option.iter(fun d->d.Dispose())

  let mapFind key m=match Map.tryFind key m with
                    |Some v->v
                    |_->failwithf "not found key=[%s]" key

  let nl=Environment.NewLine
  let repB (s:string)=s.Replace("{","{{").Replace("}","}}")

  let memoize f =
    let cache = Dictionary<_, _>()
    fun x ->
        if cache.ContainsKey(x) then cache.[x]
        else let res = f x
             cache.[x] <- res
             res
  let memoizeFunc (f:Func<'a,'b>)=memoize (fun key->f.Invoke key)

  let mapFst f (f1,f2)=f f1,f2
  let mapSnd f (f1,f2)=f1,f f2

  let someOr vOpt orV=match vOpt with Some v->v|_->orV

  let as12 v=Choice1Of2 v
  let as22 v=Choice2Of2 v
  let as13 v=Choice1Of3 v
  let as23 v=Choice2Of3 v
  let as33 v=Choice3Of3 v

//  let if' b (tv,fv)=if b then tv else fv

  let someValue=function
    |Some p->p
    |_->failwith "opt must be some"

  type ResultCommon<'a,'b>=Success of 'a|Error of 'b
  type Subject<'a>(v:'a) as this=
    let mutable _v=v
    let _evt=Event<'a>()
    let mutable _obs=[]:IObserver<'a> list
    let propChanged=Event<_,_>()
    let raisePropChanged propName=
      propChanged.Trigger(this,PropertyChangedEventArgs(propName))
    interface INotifyPropertyChanged with
      [<CLIEvent>]
      member x.PropertyChanged=propChanged.Publish
    member this.AsObservable()=
      {new IObservable<'a> with
        member this.Subscribe ob=
          _obs<-ob::_obs
          ob.OnNext _v
          {new IDisposable with
            member this.Dispose()=_obs<-_obs|>List.filter((<>)ob)}}
    [<CLIEvent>]
    member this.Changed=_evt.Publish
    member this.Value with get()=_v 
                      and set v=_v<-v
                                //値が変わった時にしかイベント上げないようにするのはチェック必要。
                                //そう依存してるようになってること自体よろしくないけど・・・
                                _evt.Trigger v
                                for o in _obs do o.OnNext v
                                raisePropChanged "Value"
  let sub v=Subject v
  let setSub (sub:Subject<_>) v=sub.Value<-v
  let asOb (sub:Subject<_>)=sub.AsObservable()
  let weakSubscribe' key f ob=
    let weakRef=ref Unchecked.defaultof<WeakReference<_>>
    let disp=ref Unchecked.defaultof<IDisposable>
    let dispose (d:IDisposable)=d.Dispose()
    let s={new IDisposable with member this.Dispose()=dispose !disp}
    weakRef:=new WeakReference<_>(s)
    disp:=ob|>Observable.subscribe(fun x->
      let mutable d=Unchecked.defaultof<IDisposable>
      if (!weakRef).TryGetTarget(&d) then f x
      else
        Log.logWarn<|repB(sprintf "###########garbaged so unsub key=%s obV=%A" key  x)
        dispose !disp
    )
    s
  let weakSubscribe f ob=weakSubscribe' "---" f ob
  let weakObToSub' key (ob:IObservable<'a>)=
    let sub=Subject(Unchecked.defaultof<'a>)
    let d=ob|>weakSubscribe' key (setSub sub)
    sub,d
  let weakObToSub ob=weakObToSub' "---" ob
  let strongObToSub(ob:IObservable<'a>)=
    let sub=Subject(Unchecked.defaultof<'a>)
    let d=ob|>Observable.subscribe(setSub sub)
    sub,d
  //let toUnitSub ob=ob|>Observable.map(fun _->())
  let toUnitOb ob=ob|>Observable.map(fun _->())
  let subToUnitOb (sub:Subject<_>)=sub.AsObservable()|>toUnitOb


//  let kvKey (kv:KeyValuePair<_,_>)=kv.Key
//  let kvVal (kv:KeyValuePair<_,_>)=kv.Value
//  let kvTpl (kv:KeyValuePair<_,_>)=kv.Key,kv.Value