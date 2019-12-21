
open System
open System.Threading

let createTimer timerInterval eventHandler =
    let timer = new System.Timers.Timer(float timerInterval)
    timer.AutoReset <- true
    timer.Elapsed.Add eventHandler

    async {
        timer.Start()
        do! Async.Sleep 5000
        timer.Stop() }

let basicHandler _ = printfn "tick %A" DateTime.Now
let basicTimer1 = createTimer 1000 basicHandler
Async.RunSynchronously basicTimer1

let createTimerAndObservable timerInterval =
    let timer = new System.Timers.Timer(float timerInterval)
    timer.AutoReset <- true

    let observable = timer.Elapsed

    let task = async {
        timer.Start()
        do! Async.Sleep 5000
        timer.Stop() }

    (task, observable)

let basicTimer2, timerEventStream = createTimerAndObservable 1000

timerEventStream
|> Observable.subscribe (fun _ -> printfn "tick %A" DateTime.Now)

Async.RunSynchronously basicTimer2

type ImperativeTimerCount() =
    let mutable count = 0

    member this.handleEvent _ =
        count <- count + 1
        printfn "timer ticked with count %i" count

let handler = new ImperativeTimerCount()

let timerCount1 = createTimer 500 handler.handleEvent

Async.RunSynchronously timerCount1

let timerCount2, timerEventStream2 = createTimerAndObservable 500

timerEventStream2
|> Observable.scan (fun count _ -> count + 1) 0
|> Observable.subscribe (fun count -> printfn "timer ticked with count %i" count)

Async.RunSynchronously timerCount2

type FizzBuzzEvent = { label: int; time: DateTime }

let areSimultaneous (earlierEvent, laterEvent) =
    let { label = _; time = t1 } = earlierEvent
    let { label = _; time = t2 } = laterEvent
    t2.Subtract(t1).Milliseconds < 50

type ImperativeFizzBuzzHandler() =
    let mutable previousEvent: FizzBuzzEvent option = None

    let printEvent thisEvent =
        let { label = id; time = t } = thisEvent
        printf "[%i] %i.%03i" id t.Second t.Millisecond
        let simultaneous = previousEvent.IsSome && areSimultaneous (previousEvent.Value, thisEvent)
        if simultaneous then printfn "FizzBuzz"
        elif id = 3 then printfn "Fizz"
        elif id = 5 then printfn "Buzz"

    member this.handleEvent3 eventArgs =
        let event = { label = 3; time = DateTime.Now }
        printEvent event
        previousEvent <- Some event

    member this.handleEvent5 eventArgs =
        let event = { label = 5; time = DateTime.Now }
        printEvent event
        previousEvent <- Some event

let handler' = new ImperativeFizzBuzzHandler()

let timer3 = createTimer 300 handler'.handleEvent3
let timer5 = createTimer 500 handler'.handleEvent5

[ timer3; timer5; ]
|> Async.Parallel
|> Async.RunSynchronously

let timer3', timerEventStream3 = createTimerAndObservable 300
let timer5', timerEventStream5 = createTimerAndObservable 500

let eventStream3 =
    timerEventStream3
    |> Observable.map (fun _ -> { label = 3; time = DateTime.Now })

let eventStream5 =
    timerEventStream5
    |> Observable.map (fun _ -> { label = 5; time = DateTime.Now })

let combinedStream =
    Observable.merge eventStream3 eventStream5

let pairwiseStream =
    combinedStream |> Observable.pairwise

let simultaneousStream, nonSimultaneousStream =
    pairwiseStream |> Observable.partition areSimultaneous

let fizzStream, buzzStream =
    nonSimultaneousStream
    |> Observable.map (fun (ev1, _) -> ev1)
    |> Observable.partition (fun { label = id } -> id = 3)

combinedStream
|> Observable.subscribe (fun { label = id; time = t } -> printfn "[%i] %i.%03i" id t.Second t.Millisecond)

simultaneousStream
|> Observable.subscribe (fun _ -> printfn "FizzBuzz")

fizzStream
|> Observable.subscribe (fun _ -> printfn "Fizz")

buzzStream
|> Observable.subscribe (fun _ -> printfn "Buzz")

[ timer3'; timer5' ]
|> Async.Parallel
|> Async.RunSynchronously