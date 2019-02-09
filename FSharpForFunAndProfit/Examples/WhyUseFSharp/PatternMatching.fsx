
let firstPart, secondPart, _ = (1, 2, 3)

let elem1::elem2::rest = [ 1..10 ]

let listMatcher aList =
    match aList with
    | [] -> printfn "the list is empty"
    | [firstElement] -> printfn "the list has one element %A" firstElement
    | [first; second] -> printfn "the list is %A and %A" first second
    | _ -> printfn "the list has more than two elements"

listMatcher [ 1; 2; 3; 4 ]
listMatcher [ 1; 2 ]
listMatcher [ 1 ]
listMatcher [ ]

type Address = { Street: string; City: string }
type Customer = { Id: int; Name: string; Address: Address }

let customer1 = { Id = 1; Name = "Bob"; Address = { Street = "123 Main"; City = "NY" } }

let { Name = name1 } = customer1
printfn "The customer is called %s" name1

let { Id = id2; Name = name2 } = customer1
printfn "The customer is called %s and has id %i" name2 id2

let { Name = name3; Address = { Street = street3 } } = customer1
printfn "The customer is called %s and lives on %s" name3 street3