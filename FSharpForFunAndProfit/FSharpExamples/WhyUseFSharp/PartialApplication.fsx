
let add x y = x + y

let z = add 1 2

let add42 = add 42

add42 2
add42 3

let genericLogger before after anyFunc input =
    before input
    let result = anyFunc input
    after result
    result

let add1 input = input + 1

genericLogger
    (fun x -> printf "before=%i. " x)
    (fun x -> printfn "after=%i." x)
    add1
    2

genericLogger
    (fun x -> printf "started with=%i" x)
    (fun x -> printfn " ended with=%i" x)
    add1
    2

let add1WithConsoleLogging =
    genericLogger
        (fun x -> printf "before=%i. " x)
        (fun x -> printfn "after=%i." x)
        add1

add1WithConsoleLogging 2
add1WithConsoleLogging 3
add1WithConsoleLogging 4
[ 1..5 ] |> List.map add1WithConsoleLogging