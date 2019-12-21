
open System

let userTimerWithCallback =
    let event = new System.Threading.AutoResetEvent(false)

    let timer = new System.Timers.Timer(2000.0)
    timer.Elapsed.Add (fun _ -> event.Set() |> ignore)

    printfn "Waiting for timer at %O" DateTime.Now.TimeOfDay
    timer.Start()

    printfn "Doing something useful while waiting for event"

    event.WaitOne() |> ignore

    printfn "Timer ticked at %O" DateTime.Now.TimeOfDay

let userTimerWithAsync =
    let timer = new System.Timers.Timer(2000.0)
    let timerEvent = Async.AwaitEvent (timer.Elapsed) |> Async.Ignore

    printfn "Waiting for timer at %O" DateTime.Now.TimeOfDay
    timer.Start()

    printfn "Doing something useful while waiting for event"

    Async.RunSynchronously timerEvent

    printfn "Timer ticked at %O" DateTime.Now.TimeOfDay

let sleepWorkflow = async {
    printfn "Starting sleep workflow at %O" DateTime.Now.TimeOfDay
    do! Async.Sleep 2000
    printfn "Finished sleep workflow at %O" DateTime.Now.TimeOfDay }

Async.RunSynchronously sleepWorkflow

let nestedWorkflow = async {
    printfn "Starting parent"
    let! childWorkflow = Async.StartChild sleepWorkflow
    
    do! Async.Sleep 100
    printfn "Doing something useful while waiting"
    
    let! result = childWorkflow
    printfn "Finished parent" }

Async.RunSynchronously nestedWorkflow

let testLoop = async {
    for i in [ 1..100 ] do
        printf "%i before.." i
        do! Async.Sleep 10
        printfn "..after" }

open System.Threading

let cancellationSource = new CancellationTokenSource()

Async.Start (testLoop, cancellationSource.Token)

Thread.Sleep(200);

cancellationSource.Cancel()

/////////////////////////////////////////////////////////////////////////////

open System.Net
open System
open System.IO

let fetchUrl url =
    let req = WebRequest.Create(Uri url)
    use resp = req.GetResponse()
    use stream = resp.GetResponseStream()
    use reader = new IO.StreamReader(stream)
    let html = reader.ReadToEnd()
    printfn "finished downloading %s" url

let sites =
    [ "http://www.bing.com";
      "http://www.google.com";
      "http://www.microsoft.com";
      "http://www.amazon.com";
      "http://www.yahoo.com" ]

#time
sites
|> List.map fetchUrl
#time

let fetchUrlAsync url = async {
    let req = WebRequest.Create(Uri url)
    use! resp = req.AsyncGetResponse()
    use stream = resp.GetResponseStream()
    use reader = new IO.StreamReader(stream)
    let html = reader.ReadToEnd()
    printfn "finished downloading %s" url }

#time
sites
|> List.map fetchUrlAsync
|> Async.Parallel
|> Async.RunSynchronously
#time

/////////////////////////////////////////////////////////////////////////////

let childTask() =
    for i in [ 1..2500 ] do
        for i in [ 1..1000 ] do
            do "Hello".Contains("H") |> ignore

#time
childTask()
#time

let parentTask =
    childTask
    |> List.replicate 20
    |> List.reduce (>>)

#time
parentTask()
#time

let asyncChildTask = async { return childTask() }

let asyncParentTask =
    asyncChildTask
    |> List.replicate 20
    |> Async.Parallel

#time
asyncParentTask |> Async.RunSynchronously
#time