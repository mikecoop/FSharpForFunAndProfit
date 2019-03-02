
type Book = { title: string; price: decimal; }

type ChocolateType = Dark | Milk | SeventyPercent
type Chocolate = { chocType: ChocolateType; price: decimal; }

type WrappingPaperStyle =
    | HappyBirthday
    | HappyHolidays
    | SolidColor

type Gift =
    | Book of Book
    | Chocolate of Chocolate
    | Wrapped of Gift * WrappingPaperStyle
    | Boxed of Gift
    | WithACard of Gift * message:string

// A Book
let wolfHall = { title = "Wolf Hall"; price = 20m }
// A Chocolate
let yummyChoc = { chocType = SeventyPercent; price = 5m }
// A Gift
let birthdayPresent = WithACard (Wrapped (Book wolfHall, HappyBirthday), "Happy Birthday")
// A Gift
let christmasPresent = Wrapped (Boxed (Chocolate yummyChoc), HappyHolidays)

let rec cataGift fBook fChocolate fWrapped fBox fCard gift :'r =
    let recurse = cataGift fBook fChocolate fWrapped fBox fCard
    match gift with
    | Book book ->
        fBook book
    | Chocolate choc ->
        fChocolate choc
    | Wrapped (gift, style) ->
        fWrapped (recurse gift, style)
    | Boxed gift ->
        fBox (recurse gift)
    | WithACard (gift, message) ->
        fCard (recurse gift, message)

let totalCostUsingCata gift =
    let fBook (book:Book) =
        book.price
    let fChocolate (choc:Chocolate) =
        choc.price
    let fWrapped (innerCost, style) =
        innerCost + 0.5m
    let fBox innerCost =
        innerCost + 1.0m
    let fCard (innerCost, message) =
        innerCost + 2.0m

    cataGift fBook fChocolate fWrapped fBox fCard gift

let deeplyNestedBox depth =
    let rec loop depth boxSoFar =
        match depth with
        | 0 -> boxSoFar
        | n -> loop (n - 1) (Boxed boxSoFar)
    loop depth (Book wolfHall)

deeplyNestedBox 5

deeplyNestedBox 10 |> totalCostUsingCata
deeplyNestedBox 100 |> totalCostUsingCata
deeplyNestedBox 1000 |> totalCostUsingCata

let rec totalCostUsingAcc costSoFar gift =
    match gift with
    | Book book ->
        costSoFar + book.price
    | Chocolate choc ->
        costSoFar + choc.price
    | Wrapped (innerGift, _) ->
        let newCostSoFar = costSoFar + 0.5m
        totalCostUsingAcc newCostSoFar innerGift
    | Boxed innerGift ->
        let newCostSoFar = costSoFar + 1.0m
        totalCostUsingAcc newCostSoFar innerGift
    | WithACard (innerGift, _) ->
        let newCostSoFar = costSoFar + 2.0m
        totalCostUsingAcc newCostSoFar innerGift

let rec foldGift fBook fChocolate fWrapped fBox fCard acc gift : 'r =
    let recurse = foldGift fBook fChocolate fWrapped fBox fCard
    match gift with
    | Book book ->
        let finalAcc = fBook acc book
        finalAcc
    | Chocolate choc ->
        let finalAcc = fChocolate acc choc
        finalAcc
    | Wrapped (innerGift, style) ->
        let newAcc = fWrapped acc style
        recurse newAcc innerGift
    | Boxed innerGift ->
        let newAcc = fBox acc
        recurse newAcc innerGift
    | WithACard (innerGift, message) ->
        let newAcc = fCard acc message
        recurse newAcc innerGift

let totalCostUsingFold gift =
    let fBook costSoFar (book:Book) =
        costSoFar + book.price
    let fChocolate costSoFar (choc:Chocolate) =
        costSoFar + choc.price
    let fWrapped costSoFar style =
        costSoFar + 0.5m
    let fBox costSoFar =
        costSoFar + 1.0m
    let fCard costSoFar message =
        costSoFar + 2.0m

    let initialAcc = 0m

    foldGift fBook fChocolate fWrapped fBox fCard initialAcc gift

deeplyNestedBox 10000 |> totalCostUsingFold
deeplyNestedBox 100000 |> totalCostUsingFold

let descriptionUsingFold gift =
    let fBook descriptionSoFar (book:Book) =
        sprintf "'%s' %s" book.title descriptionSoFar
    let fChocolate descriptionSoFar (choc:Chocolate) =
        sprintf "%A chocolate %s" choc.chocType descriptionSoFar
    let fWrapped descriptionSoFar style =
        sprintf "%s wrapped in %A paper" descriptionSoFar style
    let fBox descriptionSoFar =
        sprintf "%s in a box" descriptionSoFar
    let fCard descriptionSoFar message =
        sprintf "%s with a card saying '%s'" descriptionSoFar message

    let initialAcc = ""

    foldGift fBook fChocolate fWrapped fBox fCard initialAcc gift

let descriptionUsingFoldWithGenerator gift =
    
    let fBook descriptionGenerator (book:Book) =
        descriptionGenerator (sprintf "'%s'" book.title)

    let fChocolate descriptionGenerator (choc:Chocolate) =
        descriptionGenerator (sprintf "%A chocolate" choc.chocType)

    let fWrapped descriptionGenerator style =
        fun innerText ->
            let newInnerText = sprintf "%s wrapped in %A paper" innerText style
            descriptionGenerator newInnerText

    let fBox descriptionGenerator =
        fun innerText ->
            let newInnerText = sprintf "%s in a box" innerText
            descriptionGenerator newInnerText

    let fCard descriptionGenerator message =
        fun innerText ->
            let newInnerText = sprintf "%s with a card saying '%s'" innerText message
            descriptionGenerator newInnerText

    let initialAcc = fun innerText -> innerText

    foldGift fBook fChocolate fWrapped fBox fCard initialAcc gift

let rec foldbackGift fBook fChocolate fWrapped fBox fCard gift generator : 'r =
    let recurse = foldbackGift fBook fChocolate fWrapped fBox fCard
    match gift with
    | Book book ->
        generator (fBook book)
    | Chocolate choc ->
        generator (fChocolate choc)
    | Wrapped (innerGift, style) ->
        let newGenerator innerVal =
            let newInnerVal = fWrapped innerVal style
            generator newInnerVal
        recurse innerGift newGenerator
    | Boxed innerGift ->
        let newGenerator innerVal =
            let newInnerVal = fBox innerVal
            generator newInnerVal
        recurse innerGift newGenerator
    | WithACard (innerGift, message) ->
        let newGenerator innerVal =
            let newInnerVal = fCard innerVal message
            generator newInnerVal
        recurse innerGift newGenerator

let descriptionUsingFoldBack gift = 
    let fBook (book:Book) =
        sprintf "'%s'" book.title
    let fChocolate (choc:Chocolate) =
        sprintf "%A chocolate" choc.chocType
    let fWrapped innerText style =
        sprintf "%s wrapped in %A paper" innerText style
    let fBox innerText =
        sprintf "%s in a box" innerText
    let fCard innerText message =
        sprintf "%s with a card saying '%s'" innerText message
    let initialAcc = fun innerText -> innerText
    foldbackGift fBook fChocolate fWrapped fBox fCard gift initialAcc

birthdayPresent |> descriptionUsingFold
birthdayPresent |> descriptionUsingFoldWithGenerator
birthdayPresent |> descriptionUsingFoldBack
christmasPresent |> descriptionUsingFold
christmasPresent |> descriptionUsingFoldWithGenerator
christmasPresent |> descriptionUsingFoldBack