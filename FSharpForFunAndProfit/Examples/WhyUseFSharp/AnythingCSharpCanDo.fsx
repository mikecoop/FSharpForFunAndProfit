
/////////////////////////////////////////////////////////////
// Classes and interfaces
/////////////////////////////////////////////////////////////

type IEnumerator<'a> =
    abstract member Current : 'a
    abstract MoveNext : unit -> bool

[<AbstractClass>]
type Shape() =
    abstract member Width : int with get
    abstract member Height : int with get
    member this.BoundingArea = this.Height * this.Width
    abstract member Print : unit -> unit
    default this.Print() = printfn "I'm a shape"

type Rectangle(x: int, y:int) =
    inherit Shape()
    override this.Width = x
    override this.Height = y
    override this.Print() = printfn "I'm a Rectangle"

let r = Rectangle(2, 3)
printfn "The width is %i" r.Width
printfn "The area is %i" r.BoundingArea
r.Print()

type Circle(rad: int) =
    inherit Shape()
    let mutable radius = rad
    override this.Width = radius * 2
    override this.Height = radius * 2
    new() = Circle(10)
    member this.Radius
        with get() = radius
        and set(value) = radius <- value

let c1 = Circle()
printfn "The width is %i" c1.Width
let c2 = Circle(2)
printfn "The width is %i" c2.Width

c2.Radius <- 3
printfn "The width is %i" c2.Width

/////////////////////////////////////////////////////////////
// Generics
/////////////////////////////////////////////////////////////

type KeyValeuPair<'a, 'b>(key: 'a, value: 'b) =
    member this.Key = key
    member this.Value = value

type Container<'a, 'b
    when 'a : equality
    and 'b :> System.Collections.ICollection>
    (name: 'a, values: 'b) =
    member this.Name = name
    member this.Values = values

/////////////////////////////////////////////////////////////
// Structs
/////////////////////////////////////////////////////////////

type Point2D =
    struct
        val X: float
        val Y: float
        new(x: float, y: float) = { X = x; Y = y }
    end

let p = Point2D()
let p2 = Point2D(2.0, 3.0)

/////////////////////////////////////////////////////////////
// Exceptions
/////////////////////////////////////////////////////////////

exception MyError of string

try
    let e = MyError("Oops!")
    raise e
with
    | MyError msg ->
        printfn "The exception error was %s" msg
    | _ ->
        printfn "Some other exception"

/////////////////////////////////////////////////////////////
// Extension methods
/////////////////////////////////////////////////////////////

type System.String with
    member this.StartsWithA = this.StartsWith "A"

let s = "Alice"
printfn "'%s' starts with an 'A' = %A"

type System.Int32 with
    member this.IsEven = this % 2 = 0

let i = 20
if i.IsEven then printfn "'%i' is even" i

/////////////////////////////////////////////////////////////
// Parameter arrays
/////////////////////////////////////////////////////////////

open System
type MyConsole() =
    member this.WriteLine([<ParamArray>] args: Object[]) =
        for arg in args do
            printfn "%A" arg

let cons = new MyConsole()
cons.WriteLine("abc", 42, 3.14, true)

/////////////////////////////////////////////////////////////
// Events
/////////////////////////////////////////////////////////////

type MyButton() =
    let clickEvent = new Event<_>()

    [<CLIEvent>]
    member this.OnClick = clickEvent.Publish

    member this.TestEvent(arg) =
        clickEvent.Trigger(this, arg)

let myButton = new MyButton()
myButton.OnClick.Add(fun (sender, arg) ->
    printfn "Click event with arg=%O" arg)

myButton.TestEvent("Hello World!")

/////////////////////////////////////////////////////////////
// Enums
/////////////////////////////////////////////////////////////

type Color = | Red = 1 | Greed = 2 | Blue = 3

let color1 = Color.Red
let color2:Color = enum 2
let color3 = System.Enum.Parse(typeof<Color>, "Green") :?> Color