#load "RecursiveTypes6-1.fsx"
#r "System.Runtime.Serialization.dll"

open ``RecursiveTypes6-1``
open System.Runtime.Serialization
open System.Runtime.Serialization.Json

type Book = { Title: string; Price: decimal }
type ChocolateType = Dark | Milk | SeventyPercent
type Chocolate = { ChocType: ChocolateType; Price: decimal }

type WrappingPaperStyle =
    | HappyBirthday
    | HappyHolidays
    | SolidColor

type GiftContents =
    | Book of Book
    | Chocolate of Chocolate

type GiftDecoration =
    | Wrapped of WrappingPaperStyle
    | Boxed
    | WithACard of string

type Gift = Tree<GiftContents, GiftDecoration>

let fromBook book =
    LeafNode (Book book)

let fromChoc choc =
    LeafNode (Chocolate choc)

let wrapInPaper paperStyle innerGift =
    let container = Wrapped paperStyle
    InternalNode (container, [ innerGift ])

let putInBox innerGift =
    let container = Boxed
    InternalNode (container, [ innerGift ])

let withCard message innerGift =
    let container = WithACard message
    InternalNode (container, [ innerGift ])

let putTwoThingsInBox innerGift innerGift2 =
    let container = Boxed
    InternalNode (container, [ innerGift; innerGift2 ])

let wolfHall = { Title = "Wolf Hall"; Price = 20m }
let yummyChoc = { ChocType = SeventyPercent; Price = 5m }

let birthdayPresent =
    wolfHall
    |> fromBook
    |> wrapInPaper HappyBirthday
    |> withCard "Happy Birthday"

let christmasPresent =
    yummyChoc
    |> fromChoc
    |> putInBox
    |> wrapInPaper HappyHolidays

let twoBirthdayPresents =
    let thing1 = wolfHall |> fromBook
    let thing2 = yummyChoc |> fromChoc
    putTwoThingsInBox thing1 thing2
    |> wrapInPaper HappyHolidays

let twoWrappedPresentsInBox =
    let thing1 = wolfHall |> fromBook |> wrapInPaper HappyHolidays
    let thing2 = yummyChoc |> fromChoc |> wrapInPaper HappyBirthday
    putTwoThingsInBox thing1 thing2

let description gift =
    let fLeaf leafData =
        match leafData with
        | Book book ->
            sprintf "'%s'" book.Title
        | Chocolate choc ->
            sprintf "%A chocolate" choc.ChocType

    let fNode nodeData innerTexts =
        let innerText = String.concat " & " innerTexts
        match nodeData with
        | Wrapped style ->
            sprintf "%s wrapped in %A paper" innerText style
        | Boxed ->
            sprintf "%s in a box" innerText
        | WithACard message ->
            sprintf "%s with a card saying '%s'" innerText message

    Tree.cata fLeaf fNode gift

birthdayPresent |> description
christmasPresent |> description
twoBirthdayPresents |> description
twoWrappedPresentsInBox |> description

[<CLIMutableAttribute>]
type GiftContentsDto = {
    Discriminator: string
    BookTitle: string
    ChocolateType: string
    Price: decimal }

[<CLIMutableAttribute>]
type GiftDecorationDto = {
    Discriminator: string
    WrappingPaperStyle: string
    Message: string }

type GiftDto = Tree<GiftContentsDto, GiftDecorationDto>

let giftToDto (gift:Gift) : GiftDto =
    let fLeaf leafData : GiftContentsDto =
        match leafData with
        | Book book ->
            { Discriminator = "Book"; BookTitle = book.Title; ChocolateType = null; Price = book.Price }
        | Chocolate choc ->
            let chocolateType = sprintf "%A" choc.ChocType
            { Discriminator = "Chocolate"; BookTitle = null; ChocolateType = chocolateType; Price = choc.Price }

    let fNode nodeData : GiftDecorationDto =
        match nodeData with
        | Wrapped style ->
            let wrappingPaperStyle = sprintf "%A" style
            { Discriminator = "Wrapped"; WrappingPaperStyle = wrappingPaperStyle; Message = null }
        | Boxed ->
            { Discriminator = "Boxed"; WrappingPaperStyle = null; Message = null }
        | WithACard message ->
            { Discriminator = "WithACard"; WrappingPaperStyle = null; Message = message }

    Tree.map fLeaf fNode gift

[<CLIMutableAttribute>]
type TreeDto<'LeafData, 'NodeData> = {
    LeafData: 'LeafData
    NodeData: 'NodeData
    SubTrees: TreeDto<'LeafData, 'NodeData> [ ] }

let treeToDto tree : TreeDto<'LeafData, 'NodeData> =
    let fLeaf leafData =
        let nodeData = Unchecked.defaultof<'NodeData>
        let subtrees = [| |]
        { LeafData = leafData; NodeData = nodeData; SubTrees = subtrees }
    let fNode nodeData subtrees =
        let leafData = Unchecked.defaultof<'NodeData>
        let subtrees = subtrees |> Seq.toArray
        { LeafData = leafData; NodeData = nodeData; SubTrees = subtrees }

    Tree.cata fLeaf fNode tree

let toJson (o:'a) =
    let serializer = new DataContractJsonSerializer(typeof<'a>)
    let encoding = System.Text.UTF8Encoding()
    use stream = new System.IO.MemoryStream()
    serializer.WriteObject(stream, o)
    stream.Close()
    encoding.GetString(stream.ToArray())

let goodJson = christmasPresent |> giftToDto |> treeToDto |> toJson