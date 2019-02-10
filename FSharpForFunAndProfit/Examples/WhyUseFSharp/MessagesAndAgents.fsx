
let printerAgent = MailboxProcessor.Start (fun inbox ->
    let rec messageLoop() = async {
        let! message = inbox.Receive()
        
        printfn "message is %s" message
        
        return! messageLoop() }
        
    messageLoop() )

printerAgent.Post "hello"
printerAgent.Post "hello again"
printerAgent.Post "hello a third time"

open System
open System.Threading
open System.Diagnostics
open System.Windows.Forms

type Utility() =
    static let rand = new Random()

    static member RandomSleep() =
        let ms = rand.Next(1, 10)
        Thread.Sleep ms

type LockedCounter() =
    static let _lock = new Object()

    static let mutable count = 0
    static let mutable sum = 0

    static let updateState i =
        sum <- sum + i
        count <- count + 1
        printfn "Count is: %i. Sum is: %i" count sum
        Utility.RandomSleep()

    static member Add i =
        let stopwatch = new Stopwatch()
        stopwatch.Start()

        lock _lock (fun () ->
            stopwatch.Stop()
            printfn "Client waiting %i" stopwatch.ElapsedMilliseconds
            updateState i )

LockedCounter.Add 4
LockedCounter.Add 5

let makeCountingTask addFunction taskId = async {
    let name = sprintf "Task%i" taskId
    for i in [ 1..3 ] do
        addFunction i }

let task = makeCountingTask LockedCounter.Add 1
Async.RunSynchronously task

let lockedExample5 =
    [ 1..10 ]
    |> List.map (fun i -> makeCountingTask LockedCounter.Add i)
    |> Async.Parallel
    |> Async.RunSynchronously
    |> ignore

type MessageBasedCounter() =
    static let updateState (count, sum) msg =
        let newSum = sum + msg
        let newCount = count + 1
        printfn "Count is: %i. Sum is %i." newCount newSum

        Utility.RandomSleep()

        (newCount, newSum)

    static let agent = MailboxProcessor.Start(fun inbox ->
        let rec messageLoop oldState = async {
            let! msg = inbox.Receive()
            let newState = updateState oldState msg
            return! messageLoop newState }
            
        messageLoop (0, 0) )

    static member Add i = agent.Post i

MessageBasedCounter.Add 4
MessageBasedCounter.Add 5

let task' = makeCountingTask MessageBasedCounter.Add 1
Async.RunSynchronously task

let messageExample5 =
    [ 1..5 ]
    |> List.map (fun i -> makeCountingTask MessageBasedCounter.Add i)
    |> Async.Parallel
    |> Async.RunSynchronously
    |> ignore

let slowConsoleWrite msg =
    msg |> String.iter (fun ch -> 
        System.Threading.Thread.Sleep(1)
        System.Console.Write ch )

slowConsoleWrite "abc"

let makeTask logger taskId = async {
    let name = sprintf "Task%i" taskId
    for i in [ 1..3 ] do
        let msg = sprintf "-%s:Loop%i-" name i
        logger msg }

let task'' = makeTask slowConsoleWrite 1
Async.RunSynchronously task''

type UnserializedLogger() =
    member this.Log msg = slowConsoleWrite msg

let unserializedLogger = UnserializedLogger()
unserializedLogger.Log "hello"

let unserializedExample =
    let logger = new UnserializedLogger()
    [ 1..5 ]
    |> List.map (fun i -> makeTask logger.Log i)
    |> Async.Parallel
    |> Async.RunSynchronously
    |> ignore

type SerializedLogger() =
    let agent = MailboxProcessor.Start(fun inbox ->
        let rec messageLoop () = async {
            let! msg = inbox.Receive()
            slowConsoleWrite msg
            return! messageLoop () }
        messageLoop () )

    member this.Log msg = agent.Post msg

let serializedLogger = SerializedLogger()
serializedLogger.Log "hello"

let serializedExample =
    let logger = new SerializedLogger()
    [ 1..5 ]
    |> List.map (fun i -> makeTask logger.Log i)
    |> Async.Parallel
    |> Async.RunSynchronously
    |> ignore