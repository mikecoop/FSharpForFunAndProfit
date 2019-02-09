
type DateScale = Hour | Hours | Day | Days | Week | Weeks
type DateDirection = Ago | Hence

let getDate interval scale direction =
    let absHours = match scale with
                   | Hour | Hours -> 1 * interval
                   | Day | Days -> 24 * interval
                   | Week | Weeks -> 24 * 7 * interval
    let signedHours = match direction with
                      | Ago -> -1 * absHours
                      | Hence -> absHours
    System.DateTime.Now.AddHours(float signedHours)

let example1 = getDate 5 Hours Ago
let example2 = getDate 1 Hour Hence

type FluentShape =
    { label : string
      color : string
      onClick : FluentShape -> FluentShape }

let defaultShape = 
    { label = ""; color = ""; onClick = fun shape -> shape }

let click shape =
    shape.onClick shape

let display shape =
    printfn "My label=%s and my color=%s" shape.label shape.color
    shape

let setLabel label shape =
    { shape with FluentShape.label = label }

let setColor color shape =
    { shape with FluentShape.color = color }

let appendClickAction action shape =
    { shape with FluentShape.onClick = shape.onClick >> action }

let setRedBox = setColor "red" >> setLabel "box"
let setBlueBox = setRedBox >> setColor "blue"
let changeColorOnClick color = appendClickAction (setColor color)

let redBox = defaultShape |> setRedBox
let blueBox = defaultShape |> setBlueBox

redBox
    |> display
    |> changeColorOnClick "green"
    |> click
    |> display

blueBox
    |> display
    |> appendClickAction (setLabel "box2" >> setColor "green")
    |> click
    |> display

let rainbow = [ "red"; "orange"; "yellow"; "green"; "blue"; "indigo"; "violet" ]

let showRainbow =
    let setColorAndDisplay color = setColor color >> display
    rainbow
    |> List.map setColorAndDisplay
    |> List.reduce (>>)

defaultShape |> showRainbow