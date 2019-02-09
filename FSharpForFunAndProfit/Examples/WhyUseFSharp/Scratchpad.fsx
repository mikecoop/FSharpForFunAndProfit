
let square x = x * x

let sumOfSquares n =
    [1..n] |> List.map square |> List.sum

sumOfSquares 100

let squareClone = square
let result = [1..10] |> List.map squareClone

let execFunction aFunc aParam = aFunc aParam
let result2 = execFunction square 12

////////////////////////////////////////////////////////////////

type IntAndBool = { intPart: int; boolPart: bool }
let x = { intPart = 1; boolPart = false }

type IntOrBool =
    | IntChoice of int
    | BoolChoice of bool

let y = IntChoice 42
let z = BoolChoice true

type Shape =
    | Circle of radius:int
    | Rectangle of height:int * width:int
    | Point of x:int * y:int
    | Polygon of pointList:(int * int) list

let draw shape =
    match shape with
    | Circle radius -> printfn "The cirlce has a radius of %d" radius
    | Rectangle (height, width) -> printfn "The rectangle is %d high by %d wide" height width
    | Polygon points -> printfn "The polygon is made of these points %A" points
    | _ -> printfn "I don't recognize this shape"

let circle = Circle(10)
let rect = Rectangle(4, 5)
let point = Point(2, 3)
let polygon = Polygon( [(1,1); (2,2); (3,3)] )

[circle; rect; polygon; point] |> List.iter draw

////////////////////////////////////////////////////////////////

let i = 1
let s = "hello"
let tuple = s,i
let s2,i2 = tuple
let list = [s2]

let sumLengths strList =
    strList |> List.map String.length |> List.sum

////////////////////////////////////////////////////////////////

open System

type Person = { FirstName:string; LastName:string; Dob:DateTime }
type Coord = { Lat:float; Long:float }

type TimePeriod = Hour | Day | Week | Year
type Temperature = C of int | F of int
type Appointment = OneTime of DateTime
                   | Recurring of DateTime list

////////////////////////////////////////////////////////////////

let product n =
    let initialValue = 1
    let action productSoFar x = productSoFar * x
    [1..n] |> List.fold action initialValue

product 10

let sumOfOdds n =
    let initialValue = 0
    let action sumSoFar x = if x % 2 = 0 then sumSoFar else sumSoFar + x
    [1..n] |> List.fold action initialValue

sumOfOdds 10

let alternatingSum n =
    let initalValue = (true, 0)
    let action (isNeg, sumSoFar) x = if isNeg then (false, sumSoFar - x)
                                     else (true, sumSoFar + x)
    [1..n] |> List.fold action initalValue |> snd

alternatingSum 100

////////////////////////////////////////////////////////////////

type NameAndSize = { Name : string; Size : int }
let people = [
    { Name = "Alice"; Size = 10 }
    { Name = "Bob"; Size = 1}
    { Name = "Carol"; Size = 12 }
    { Name = "David"; Size = 5} ]

people |> List.maxBy (fun item -> item.Size)
[] |> List.maxBy (fun item -> item.Size)

let maxNameAndSize list =
    match list with
    | [] -> None
    | _ -> Some(list |> List.maxBy (fun item -> item.Size))
people |> maxNameAndSize
[] |> maxNameAndSize