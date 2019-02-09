
let (|Int|_|) str =
    match System.Int32.TryParse(str) with
    | (true, int) -> Some(int)
    | (false, _) -> None

let (|Bool|_|) str =
    match System.Boolean.TryParse(str) with
    | (true, bool) -> Some(bool)
    | (false, _) -> None

let testParse str =
    match str with
    | Int i -> printfn "The value is an int '%i'" i
    | Bool b -> printfn "The value is a bool '%b'" b
    | _ -> printfn "The value '%s' is something else" str

testParse "12"
testParse "true"
testParse "abc"

open System.Text.RegularExpressions
let (|FirstRegexGroup|_|) pattern input =
    let m = Regex.Match(input, pattern)
    if (m.Success) then Some(m.Groups.[1].Value) else None

let testRegex str =
    match str with
    | FirstRegexGroup "http://(.*?)/(.*)" host ->
        printfn "The value is a url and the host is %s" host
    | FirstRegexGroup ".*?@(.*)" host ->
        printfn "The value is an email and the host is %s" host
    | _ -> printfn "The value '%s' is something else" str

testRegex "http://google.com/test"
testRegex "alice@hotmail.com"

let (|MultOf3|_|) i = if i % 3 = 0 then Some MultOf3 else None
let (|MultOf5|_|) i = if i % 5 = 0 then Some MultOf5 else None

let fizzBuzz i =
    match i with
    | MultOf3 & MultOf5 -> printf "FizzBuzz, "
    | MultOf3 -> printf "Fizz, "
    | MultOf5 -> printf "Buzz, "
    | _ -> printf "%i, " i

[ 1..20 ] |> List.iter fizzBuzz