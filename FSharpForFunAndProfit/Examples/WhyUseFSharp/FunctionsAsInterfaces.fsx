
let addingCalculator input = input + 1

let loggingCalculator innerCalculator input =
    printfn "input is %A" input
    let result = innerCalculator input
    printfn "result is %A" result
    result

let add1 input = input + 1
let times2 input = input * 2

let genericLogger anyFunc input =
    printfn "input is %A" input
    let result = anyFunc input
    printfn "result is %A" result
    result

let add1WithLogging = genericLogger add1
let times2WithLogging = genericLogger times2

add1WithLogging 3
times2WithLogging 3

[ 1..5 ] |> List.map add1WithLogging

let genericTimer anyFunc input =
    let stopwatch = System.Diagnostics.Stopwatch()
    stopwatch.Start()
    let result = anyFunc input
    printfn "elapsed ms is %A" stopwatch.ElapsedMilliseconds
    result

let add1WithTimer = genericTimer add1WithLogging

add1WithTimer 3

type Animal(noiseMakingStrategy) =
    member this.MakeNoise =
        noiseMakingStrategy() |> printfn "Making noise %s"

let meowing() = "Meow"
let cat = Animal(meowing)
cat.MakeNoise

let woofOrBark() = if (System.DateTime.Now.Second % 2 = 0)
                   then "Woof"
                   else "Bark"

let dog = Animal(woofOrBark)
dog.MakeNoise
dog.MakeNoise
System.Threading.Thread.Sleep(1000)
dog.MakeNoise