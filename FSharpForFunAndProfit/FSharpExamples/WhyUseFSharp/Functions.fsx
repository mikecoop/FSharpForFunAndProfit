open System.Web.Services.Protocols

// building blocks
let add2 x = x + 2
let mult3 x = x * 3
let square x = x * x

// test
[1..10] |> List.map add2 |> printfn "%A"
[1..10] |> List.map mult3 |> printfn "%A"
[1..10] |> List.map square |> printfn "%A"

// new composed functions
let add2ThenMult3 = add2 >> mult3
let mult3ThenSquare = mult3 >> square

// test
add2ThenMult3 5
mult3ThenSquare 5
[1..10] |> List.map add2ThenMult3 |> printfn "%A"
[1..10] |> List.map mult3ThenSquare |> printfn "%A"

// helper functions
let logMsg msg x = printf "%s%i" msg x; x
let logMsgN msg x = printfn "%s%i" msg x; x

// new composed function with improved logging
let mult3ThenSquareLogged =
    logMsg "before="
    >> mult3
    >> logMsg " after mult3="
    >> square
    >> logMsgN " result="

// test
mult3ThenSquareLogged 5
[1..10] |> List.map mult3ThenSquareLogged

let listOfFunctions =
    [ mult3
      square
      add2
      logMsgN "result=" ]

let allFunctions = List.reduce (>>) listOfFunctions

allFunctions 5