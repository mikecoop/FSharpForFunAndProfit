open System.Web.UI.WebControls

type Book = { title: string; price: decimal; }

type ChocolateType = Dark | Milk | SeventyPercent
type Chocolate = { chocType: ChocolateType; price: decimal; }

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

type Gift = { contents: GiftContents; decorations: GiftDecoration list }

let fromBook book =
    { contents = (Book book); decorations = [ ] }

let fromChoc choc =
    { contents = (Chocolate choc); decorations = [ ] }

let wrapInPaper paperStyle innerGift =
    let decoration = Wrapped paperStyle
    { innerGift with decorations = decoration :: innerGift.decorations }

let putInBox innerGift =
    let decoration = Boxed
    { innerGift with decorations = decoration :: innerGift.decorations}

let withACard message innerGift =
    let decoration = WithACard message
    { innerGift with decorations = decoration :: innerGift.decorations }

let wolfHall = { title = "Wolf Hall"; price = 20m }
let yummyChoc = { chocType = SeventyPercent; price = 5m }

let birthdayPresent =
    wolfHall
    |> fromBook
    |> wrapInPaper HappyBirthday
    |> withACard "Happy Birthday"

let christmasPresnet =
    yummyChoc
    |> fromChoc
    |> putInBox
    |> wrapInPaper HappyHolidays

let totalCost gift =
    let contentCost =
        match gift.contents with
        | Book book ->
            book.price
        | Chocolate choc ->
            choc.price
    let decorationFolder costSoFar decorationInfo =
        match decorationInfo with
        | Wrapped style ->
            costSoFar + 0.5m
        | Boxed ->
            costSoFar + 1.0m
        | WithACard message ->
            costSoFar + 2.0m

    let decorationCost =
        gift.decorations |> List.fold decorationFolder 0m

    contentCost + decorationCost

let description gift =
    let contentDescription =
        match gift.contents with
        | Book book ->
            sprintf "'%s'" book.title
        | Chocolate choc ->
            sprintf "%A chocolate" choc.chocType

    let decorationFolder decorationInfo innerText =
        match decorationInfo with
        | Wrapped style ->
            sprintf "%s wrapped in %A paper" innerText style
        | Boxed ->
            sprintf "%s in a box" innerText
        | WithACard message ->
            sprintf "%s with a card saying '%s'" innerText message

    List.foldBack decorationFolder gift.decorations contentDescription