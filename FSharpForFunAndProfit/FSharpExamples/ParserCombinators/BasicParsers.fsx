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