open Microsoft.FSharp.Core

// map function

let mapOption f opt =
    match opt with
    | None -> None
    | Some x -> Some (f x)

let rec mapList f list =
    match list with
    | [ ] -> [ ]
    | head::tail ->
        (f head) :: (mapList f tail)

let add1 x = x + 1
let add1IfSomething = Option.map add1
let add1ToEachElement = List.map add1

Some 2 |> add1IfSomething
[ 1; 2; 3 ] |> add1ToEachElement

// return function

let returnOption x = Some x

let returnList x = [ x ]

// apply function

module Option =

    let apply fOpt xOpt =
        match fOpt, xOpt with
        | Some f, Some x -> Some (f x)
        | _ -> None

module List =

    let apply (fList:('a->'b) list) (xList:'a list) =
        [ for f in fList do
          for x in xList do
            yield f x ]

let add = (+)

let resultOption =
    let (<*>) = Option.apply
    (Some add) <*> (Some 2) <*> (Some 3)

let resultList =
    let (<*>) = List.apply
    [ add ] <*> [ 1; 2 ] <*> [ 10; 20 ]

let resultOption2 =
    let (<!>) = Option.map
    let (<*>) = Option.apply
    add <!> (Some 2) <*> (Some 3)

let batman =
    let (<!>) = List.map
    let (<*>) = List.apply
    (+) <!> [ "bam"; "kapow"; "zap" ] <*> [ "!"; "!!" ]