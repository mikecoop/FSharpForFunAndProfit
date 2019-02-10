open System.Globalization

let i1success, i1 = System.Int32.TryParse("123")
if i1success then printfn "parse as %i" i1
else printfn "parse failed"

let i2success, i2 = System.Int32.TryParse("hello")
if i2success then printfn "parse as %i" i2
else printfn "parse failed"

let (d1Success, d1) = System.DateTime.TryParse("1/1/1980")
let (d2Success, d2) = System.DateTime.TryParse("hello")

let dict = new System.Collections.Generic.Dictionary<string,string>();
dict.Add("a", "hello")

let (e1success, e1) = dict.TryGetValue("a")
let (e2success, e2) = dict.TryGetValue("b")

let createReader fileName = new System.IO.StreamReader(path = fileName)

let (|Digit|Letter|Whitespace|Other|) ch =
    if System.Char.IsDigit(ch) then Digit
    elif System.Char.IsLetter(ch) then Letter
    elif System.Char.IsWhiteSpace(ch) then Whitespace
    else Other

let printChar ch =
    match ch with
    | Digit -> printfn "%c is a Digit" ch
    | Letter -> printfn "%c is a Letter" ch
    | Whitespace -> printfn "%c is a Whitespace" ch
    | _ -> printfn "%c is something else" ch

[ 'a'; 'b'; '1'; ' '; '-'; 'c' ] |> List.iter printChar

let makeResource name =
    { new System.IDisposable
      with member this.Dispose() = printfn "%s dispose" name }

let useAndDisposeResources =
    use r1 = makeResource "first resource"
    printfn "using first resource"
    for i in [ 1..3 ] do
        let resourceName = sprintf "\tinner resource %d" i1
        use temp = makeResource resourceName
        printfn "\tdo something with %s" resourceName
    use r2 = makeResource "second resource"
    printfn "using second resource"
    printfn "done."

type IAnimal =
    abstract member MakeNoise : unit -> string

let showTheNoiseAnAnimalMakes (animal: IAnimal) =
    animal.MakeNoise() |> printfn "Making noise %s"

type Cat = Felix | Socks
type Dog = Butch | Lassie

type Cat with
    member this.AsAnimal =
        { new IAnimal
          with member a.MakeNoise() = "Meow" }

type Dog with 
    member this.AsAnimal =
        { new IAnimal 
          with member a.MakeNoise() = "Woof" }

let dog = Lassie
showTheNoiseAnAnimalMakes (dog.AsAnimal)

let cat = Felix
showTheNoiseAnAnimalMakes (cat.AsAnimal)