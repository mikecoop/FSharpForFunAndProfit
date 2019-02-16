open System

type ParserLabel = string
type ParserError = string

type Position =
    { line: int
      column: int }

type InputState =
    { lines: string[]
      position: Position }

type ParserPosition =
    { currentLine: string
      line: int
      column: int }

/// Type that represents Success/Failure in parsing
type Result<'a> =
    | Success of 'a
    | Failure of ParserLabel * ParserError * ParserPosition

type Input = InputState

/// Type that wraps a parsing function
type Parser<'a> = {
    parseFn: (Input -> Result<'a * Input>)
    label: ParserLabel }

let initialPos = { line = 0; column = 0 }

let incrCol (pos:Position) =
    { pos with column = pos.column + 1 }

let incrLine (pos:Position) =
    { pos with line = pos.line + 1 }

let fromStr str =
    if String.IsNullOrEmpty(str) then
        { lines = [||]; position = initialPos }
    else
        let separators = [| "\r\n"; "\n" |]
        let lines = str.Split(separators, StringSplitOptions.None)
        { lines = lines; position = initialPos }

let currentLine inputState =
    let linePos = inputState.position.line
    if linePos < inputState.lines.Length then
        inputState.lines.[linePos]
    else
        "end of file"

let parserPositionFromInputState (inputState:InputState) =
    { currentLine = currentLine inputState
      line = inputState.position.line
      column = inputState.position.column }

let nextChar input =
    let linePos = input.position.line
    let colPos = input.position.column

    if linePos >= input.lines.Length then
        (input, None)
    else
        let currentLine = currentLine input
        if colPos < currentLine.Length then
            let char = currentLine.[colPos]
            let newPos = incrCol input.position
            let newState = { input with position = newPos }
            (newState, Some char)
        else
            let char = '\n'
            let newPos = incrLine input.position
            let newState = { input with position = newPos }
            (newState, Some char)

let printResult result =
    match result with
    | Success (value, input) ->
        printfn "%A" value
    | Failure (label, error, parserPos) ->
        let errorLine = parserPos.currentLine
        let colPos = parserPos.column
        let linePos = parserPos.line
        let failureCaret = sprintf "%*s^%s" colPos "" error
        printfn "Line:%i Col:%i Error parsing %s\n%s\n%s" linePos colPos label errorLine failureCaret

let getLabel parser =
    parser.label

let setLabel parser newLabel =
    let newInnerFn input =
        let result = parser.parseFn input
        match result with
        | Success s ->
            Success s
        | Failure (oldLabel, err, pos) ->
            Failure (newLabel, err, pos)
    { parseFn = newInnerFn; label = newLabel }

let ( <?> ) = setLabel

/// Parse a single character
let satisfy predicate label =
    let innerFn input =
        let remainingInput, charOpt = nextChar input
        match charOpt with
        | None ->
            let err = "No more input"
            let pos = parserPositionFromInputState input
            Failure (label, err, pos)
        | Some first ->
            if predicate first then
                Success (first, remainingInput)
            else
                let err = sprintf "Unexpected '%c'" first
                let pos = parserPositionFromInputState input
                Failure (label, err, pos)
    // return the "wrapped" inner function
    { parseFn = innerFn; label = label }

/// parse a char
let pchar charToMatch =
    let label = sprintf "%c" charToMatch
    let predicate ch = (ch = charToMatch)
    satisfy predicate label

let runOnInput parser input =
    parser.parseFn input

/// Run a parser with some input
let run parser inputStr =
    runOnInput parser (fromStr inputStr)

/// "bindP" takes a parser-producing function f, and a parser p
/// and passes the output of p into f, to create a new parser
let bindP f p =
    let label = "unknown"
    let innerFn input =
        let result1 = runOnInput p input
        match result1 with
        | Failure (label, err, pos) ->
            // return error from parser1
            Failure (label, err, pos)
        | Success (value1, remainingInput) ->
            // apply f to get a new parser
            let p2 = f value1
            // run parser with remaining input
            runOnInput p2 remainingInput
    { parseFn = innerFn; label = label }

/// Infix version of bindP
let ( >>= ) p f = bindP f p

/// Lift a value to a Parser
let returnP x =
    let label = "unknown"
    let innerFn input =
        // ignore the input and return x
        Success (x, input)
    // return the inner function
    { parseFn = innerFn; label = label }

/// apply a function to the value inside a parser
let mapP f =
    bindP (f >> returnP)

/// infix version of mapP
let ( <!> ) = mapP

/// "piping" version of mapP
let ( |>> ) x f = mapP f x

/// apply a wrapped function to a wrapped value
let applyP fP xP =
    fP >>= (fun f ->
    xP >>= (fun x ->
        returnP (f x) ))

/// infix version of apply
let ( <*> ) = applyP

/// lift a two parameter function to Parser World
let lift2 f xP yP =
    returnP f <*> xP <*> yP

/// Combine two parsers as "A andThen B"
let andThen p1 p2 =
    p1 >>= (fun p1Result ->
    p2 >>= (fun p2Result ->
        returnP (p1Result, p2Result) ) )

/// Infix version of andThen
let ( .>>. ) = andThen

/// Combine two parsers as "A orElse B"
let orElse p1 p2 =
    let label = sprintf "%s orElse %s" (getLabel p1) (getLabel p2)
    let innerFn input =
        // run parser1 with the input
        let result1 = runOnInput p1 input

        // test the result for Failure/Success
        match result1 with
        | Success result ->
            // if success, return the original result
            result1

        | Failure (label, err, pos) ->
            // if failed, run parser2 with the input
            let result2 = runOnInput p2 input

            // return parser2's result
            result2

    // return the inner function
    { parseFn = innerFn; label = label }

/// Infix version of orElse
let ( <|> ) = orElse

/// Choose any of a list of parsers
let choice listOfParsers =
    List.reduce ( <|> ) listOfParsers

let anyOf listOfChars =
    let label = sprintf "anyOf %A" listOfChars
    listOfChars
    |> List.map pchar
    |> choice
    <?> label

/// Convert a list of Parsers into a Parser of a list
let rec sequence parserList =
    // define the "cons" function, which is a two parameter function
    let cons head tail = head::tail

    // lift it to Parser World
    let consP = lift2 cons

    // process the list of parsers recursively
    match parserList with
    | [] ->
        returnP []
    | head :: tail ->
        consP head (sequence tail)

/// (helper) match zero or more occurences of the specified parser
let rec parseZeroOrMore parser input =
    // run parser with the input
    let firstResult = runOnInput parser input
    // test the result for Failure/Success
    match firstResult with
    | Failure (label, err, pos) ->
        // if parse fails, return empty list
        ([],input)
    | Success (firstValue,inputAfterFirstParse) ->
        // if parse succeeds, call recursively
        // to get the subsequent values
        let (subsequentValues,remainingInput) =
            parseZeroOrMore parser inputAfterFirstParse
        let values = firstValue::subsequentValues
        (values,remainingInput)

/// matches zero or more occurences of the specified parser
let many parser =
    let label = sprintf "many %s" (getLabel parser)
    let rec innerFn input =
        // parse the input -- wrap in Success as it always succeeds
        Success (parseZeroOrMore parser input)

    { parseFn = innerFn; label = label }

/// matches one or more occurences of the specified parser
let many1 p =
    p      >>= (fun head ->
    many p >>= (fun tail ->
        returnP (head::tail) ))

/// Parses an optional occurrence of p and returns an option value.
let opt p =
    let some = p |>> Some
    let none = returnP None
    some <|> none

/// Keep only the result of the left side parser
let (.>>) p1 p2 =
    // create a pair
    p1 .>>. p2
    // then only keep the first value
    |> mapP (fun (a,b) -> a)

/// Keep only the result of the right side parser
let (>>.) p1 p2 =
    // create a pair
    p1 .>>. p2
    // then only keep the second value
    |> mapP (fun (a,b) -> b)

/// Keep only the result of the middle parser
let between p1 p2 p3 =
    p1 >>. p2 .>> p3

/// Parses one or more occurrences of p separated by sep
let sepBy1 p sep =
    let sepThenP = sep >>. p
    p .>>. many sepThenP
    |>> fun (p,pList) -> p::pList

/// Parses zero or more occurrences of p separated by sep
let sepBy p sep =
    sepBy1 p sep <|> returnP []

let digitChar =
    let predicate = Char.IsDigit
    let label = "digit"
    satisfy predicate label

let rec readAllChars input =
    [ let reaminingInput, charOpt = nextChar input
      match charOpt with
      | None ->
            ()
      | Some ch ->
            yield ch
            yield! readAllChars reaminingInput ]

let charListToStr charList =
    String(List.toArray charList)

let manyChars cp =
    many cp
    |>> charListToStr

let manyChars1 cp =
    many1 cp
    |>> charListToStr

let pstring str =
    let label = str

    str
    |> List.ofSeq
    |> List.map pchar
    |> sequence
    |> mapP charListToStr
    <?> label

let whitespaceChar =
    let predicate = Char.IsWhiteSpace
    let label = "whitespace"
    satisfy predicate label

let spaces = many whitespaceChar

let spaces1 = many1 whitespaceChar

let digitChar =
    let predicate = Char.IsDigit
    let label = "digit"
    satisfy predicate label

let pint =
    let label = "integer"

    let resultToInt (sign, digits) =
        let i = digits |> int
        match sign with
        | Some ch -> -i
        | None -> i

    let digits = manyChars1 digitChar

    opt (pchar '-') .>>. digits
    |> mapP resultToInt
    <?> label

let pfloat =
    let label = "float"

    let resultToFloat (((sign, digits1), point), digits2) =
        let fl = sprintf "%s.%s" digits1 digits2 |> float
        match sign with
        | Some ch -> -fl
        | None -> fl

    let digits = manyChars1 digitChar

    opt (pchar '-') .>>. digits .>>. pchar '.' .>>. digits
    |> mapP resultToFloat
    <?> label