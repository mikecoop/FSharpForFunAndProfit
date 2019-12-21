open System

let A_Parser str =
    if String.IsNullOrEmpty(str) then
        (false, "")
    elif str.[0] = 'A' then
        let remaining = str.[1..]
        (true, remaining)
    else
        (false, str)

let inputABC = "ABC"
A_Parser inputABC

let inputZBC = "ZBC"
A_Parser inputZBC

// First implementation
let pchar (charToMatch, str) =
    if String.IsNullOrEmpty(str) then
        let msg = "No more input"
        (msg, "")
    else
        let first = str.[0]
        if first = charToMatch then
            let remaining = str.[1..]
            let msg = sprintf "Found %c" charToMatch
            (msg, remaining)
        else
            let msg = sprintf "Excpecting '%c'. Got '%c'" charToMatch first
            (msg, str)

pchar('A', inputABC)

pchar('A', inputZBC)

type Result<'a> =
    | Success of 'a
    | Failure of string

// Change output to Result type
let pchar2 (charToMatch, str) =
    if String.IsNullOrEmpty(str) then
        Failure "No more input"
    else
        let first = str.[0]
        if first = charToMatch then
            let remaining = str.[1..]
            Success (charToMatch, remaining)
        else
            let msg = sprintf "Excpecting '%c'. Got '%c'" charToMatch first
            Failure msg

pchar2('A', inputABC)

pchar2('A', inputZBC)

// Curried inputs
let pchar3 charToMatch str =
    if String.IsNullOrEmpty(str) then
        Failure "No more input"
    else
        let first = str.[0]
        if first = charToMatch then
            let remaining = str.[1..]
            Success (charToMatch, remaining)
        else
            let msg = sprintf "Excpecting '%c'. Got '%c'" charToMatch first
            Failure msg

// Inner function
let pchar4 charToMatch =
    let innerFn str =
        if String.IsNullOrEmpty(str) then
            Failure "No more input"
        else
            let first = str.[0]
            if first = charToMatch then
                let remaining = str.[1..]
                Success (charToMatch, remaining)
            else
                let msg = sprintf "Excpecting '%c'. Got '%c'" charToMatch first
                Failure msg
    innerFn

let parseA = pchar3 'A'

parseA inputABC
parseA inputZBC

type Parser<'T> = Parser of (string -> Result<'T * string>)

// Wrapped with Parser type
let pchar5 charToMatch =
    let innerFn str =
        if String.IsNullOrEmpty(str) then
            Failure "No more input"
        else
            let first = str.[0]
            if first = charToMatch then
                let remaining = str.[1..]
                Success (charToMatch, remaining)
            else
                let msg = sprintf "Excpecting '%c'. Got '%c'" charToMatch first
                Failure msg
    Parser innerFn

let run parser input =
    let (Parser innerFn) = parser
    innerFn input

let parseA' = pchar5 'A'
run parseA' inputABC

// Combining two parsers 'And Then'
let andThen parser1 parser2 =
    let innerFn input =
        let result1 = run parser1 input

        match result1 with
        | Failure err -> Failure err
        | Success (value1, remaining) ->
            let result2 = run parser2 remaining

            match result2 with
            | Failure err -> Failure err
            | Success (value2, remaining2) ->
                let newValue = (value1, value2)
                Success (newValue, remaining2)

    Parser innerFn

let ( .>>. ) = andThen

let parseA'' = pchar5 'A'
let parseB'' = pchar5 'B'

let parseAThenB = parseA'' .>>. parseB''

run parseAThenB "ABC"
run parseAThenB "ZBC"
run parseAThenB "AZC"

// Combining two parsers 'Or Else'
let orElse parser1 parser2 =
    let innerFn input =
        let result1 = run parser1 input

        match result1 with
        | Success result -> result1
        | Failure err ->
            let result2 = run parser2 input
            result2

    Parser innerFn

let ( <|> ) = orElse

let parseAOrB = parseA'' <|> parseB''

run parseAOrB "AZZ"
run parseAOrB "BZZ"
run parseAOrB "CZZ"

// Combining andThen and orElse
let parseA1 = pchar5 'A'
let parseB1 = pchar5 'B'
let parseC1 = pchar5 'C'

let bOrElseC = parseB1 <|> parseC1
let aAndThenBorC = parseA1 .>>. bOrElseC

run aAndThenBorC "ABZ"
run aAndThenBorC "ACZ"
run aAndThenBorC "QBZ"
run aAndThenBorC "AQZ"