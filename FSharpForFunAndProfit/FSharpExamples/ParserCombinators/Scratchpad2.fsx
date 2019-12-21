open System

type Result<'a> =
    | Success of 'a
    | Failure of string


type Parser<'T> = Parser of (string -> Result<'T * string>)

// Parse a single character
let pchar charToMatch =
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

/// Run a parser with some input
let run parser input =
    let (Parser innerFn) = parser
    innerFn input

/// Combining two parsers "A andThen B"
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

/// Infix version of andThen
let ( .>>. ) = andThen

/// Combining two parsers as "A orElse B"
let orElse parser1 parser2 =
    let innerFn input =
        let result1 = run parser1 input

        match result1 with
        | Success _ -> result1
        | Failure _ ->
            let result2 = run parser2 input
            result2

    Parser innerFn

/// Infix version of orElse
let ( <|> ) = orElse

/// Choose any of a list of parsers
let choice listOfParsers = List.reduce ( <|> ) listOfParsers

/// Choose any of a list of characters
let anyOf listOfChars =
    listOfChars
    |> List.map pchar
    |> choice

let mapP f parser =
    let innerFn input =
        let result = run parser input

        match result with
        | Success (value, remaining) ->
            let newValue = f value
            Success (newValue, remaining)

        | Failure err -> Failure err

    Parser innerFn

let ( <!> ) = mapP

let ( |>> ) x f = mapP f x

let parseDigit = anyOf [ '0'..'9' ]

let parseThreeDigitsAsStr =
    let tupleParser =
        parseDigit .>>. parseDigit .>>. parseDigit

    let transformTuple ((c1, c2), c3) =
        String [| c1; c2; c3; |]

    mapP transformTuple tupleParser

run parseThreeDigitsAsStr "123A"

let parseThreeDigitsAsInt =
    mapP int parseThreeDigitsAsStr

run parseThreeDigitsAsInt "123A"

let returnP x =
    let innerFn input =
        Success (x, input)
    Parser innerFn

let applyP fP xP =
    (fP .>>. xP)
    |> mapP (fun (f, x) -> f x)

let ( <*> ) = applyP

let lift2 f xP yP =
    returnP f <*> xP <*> yP

let addP = lift2 (+)

let startsWith (str:string) prefix =
    str.StartsWith(prefix)

let startsWithP = lift2 startsWith

let rec sequence parserList =
    let cons head tail = head :: tail
    let consP = lift2 cons
    match parserList with
    | [ ] -> returnP [ ]
    | head :: tail -> consP head (sequence tail)

let parsers = [ pchar 'A'; pchar 'B'; pchar 'C' ]
let combined = sequence parsers

run combined "ABCD"

let charListToStr charList =
    String (List.toArray charList)

let pstring str =
    str
    |> List.ofSeq
    |> List.map pchar
    |> sequence
    |> mapP charListToStr

let parseABC = pstring "ABC"

run parseABC "ABCDE"
run parseABC "A|CDE"
run parseABC "AB|DE"

let rec parseZeroOrMore parser input =
    let firstResult = run parser input
    match firstResult with
    | Failure _ -> ([ ], input)
    | Success (firstValue, inputAfterFirstParse) ->
        let (subsequentValues, remainingInput) =
            parseZeroOrMore parser inputAfterFirstParse
        let values = firstValue :: subsequentValues
        (values, remainingInput)

let many parser =
    let rec innerFn input =
        Success (parseZeroOrMore parser input)
    Parser innerFn

let manyA = many (pchar 'A')

run manyA "ABCD"
run manyA "AACD"
run manyA "AAAD"

run manyA "|BCD"

let manyAB = many (pstring "AB")

run manyAB "ABCD"
run manyAB "ABABCD"
run manyAB "ZCD"
run manyAB "AZCD"

let whitespaceChar = anyOf [ ' '; '\t'; '\n' ]
let whitespace = many whitespaceChar

run whitespace "ABC"
run whitespace " ABC"
run whitespace "\tABC"

let many1 parser =
    let rec innerFn input =
        let firstResult = run parser input
        match firstResult with
        | Failure err -> Failure err
        | Success (firstValue, inputAfterFirstParse) ->
            let (subsequentValues, remainingInput) =
                parseZeroOrMore parser inputAfterFirstParse
            let values = firstValue :: subsequentValues
            Success (values, remainingInput)
    Parser innerFn

let digit = anyOf [ '0'..'9' ]

let digits = many1 digit

run digits "1ABC"
run digits "12BC"
run digits "123C"
run digits "1234"
run digits "ABC"

let pint =
    let resultToInt digitList =
        String (List.toArray digitList) |> int
    let digit = anyOf [ '0'..'9' ]
    let digits = many1 digit
    digits
    |> mapP resultToInt

run pint "1ABC"
run pint "12BC"
run pint "123C"
run pint "1234"
run pint "ABC"

let opt p =
    let some = p |>> Some
    let none = returnP None
    some <|> none

let digitThenSemicolon = digit .>>. opt (pchar ';')

run digitThenSemicolon "1;"
run digitThenSemicolon "1"

let pint' =
    let resultToInt (sign, charList) =
        let i = String (List.toArray charList) |> int
        match sign with
        | Some _ -> -i
        | None -> i
    let digit = anyOf [ '0'..'9' ]
    let digits = many1 digit
    opt (pchar '-') .>>. digits
    |>> resultToInt

run pint' "123C"
run pint' "-123C"

let (.>>) p1 p2 =
    p1 .>>. p2
    |> mapP (fun (a, b) -> a)

let (>>.) p1 p2  =
    p1 .>>. p2
    |> mapP (fun (a, b) -> b)

let digitThenSemicolon' = digit .>> opt (pchar ';')

run digitThenSemicolon' "1;"
run digitThenSemicolon' "1"

let ab = pstring "AB"
let cd = pstring "CD"
let ab_cd = (ab .>> whitespace) .>>. cd

run ab_cd "AB \t\nCD"

let between p1 p2 p3 =
    p1 >>. p2 .>> p3

let pdoublequote = pchar '"'
let quotedInteger = between pdoublequote pint pdoublequote

run quotedInteger "\"1234\""
run quotedInteger "1234"

let sepBy1 p sep =
    let sepThenP = sep >>. p
    p .>>. many sepThenP
    |>> fun (p, pList) -> p :: pList

let sepBy p sep =
    sepBy1 p sep <|> returnP [ ]

let comma = pchar ','

let zeroOrMoreDigitList = sepBy digit comma
let oneOrMoreDigitList = sepBy1 digit comma

run oneOrMoreDigitList "1;"
run oneOrMoreDigitList "1,2;"
run oneOrMoreDigitList "1,2,3;"
run oneOrMoreDigitList "Z;"

run zeroOrMoreDigitList "1;"
run zeroOrMoreDigitList "1,2;"
run zeroOrMoreDigitList "1,2,3;"
run zeroOrMoreDigitList "Z;"

let bindp f p =
    let innerFn input =
        let result1 = run p input
        match result1 with
        | Failure err -> Failure err
        | Success (value1, remainingInput) ->
            let p2 = f value1
            run p2 remainingInput
    Parser innerFn

let ( >>= ) p f = bindP f p

