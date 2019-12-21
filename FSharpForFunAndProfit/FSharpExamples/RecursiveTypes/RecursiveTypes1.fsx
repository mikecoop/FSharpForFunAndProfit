
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

let rec description gift =
    match gift with
    | Book book ->
        sprintf "'%s'" book.title
    | Chocolate choc ->
        sprintf "%A chocolate" choc.chocType
    | Wrapped (innerGift, style) ->
        sprintf "%s wrapped in %A paper" (description innerGift) style
    | Boxed innerGift ->
        sprintf "%s in a box" (description innerGift)
    | WithACard (innerGift, message) ->
        sprintf "%s with a card saying '%s'" (description innerGift) message

let rec totalCost gift =
    match gift with
    | Book book ->
        book.price
    | Chocolate choc ->
        choc.price
    | Wrapped (innerGift, _) ->
        (totalCost innerGift) + 0.5m
    | Boxed innerGift ->
        (totalCost innerGift) + 1.0m
    | WithACard (innerGift, _) ->
        (totalCost innerGift) + 2.0m

let rec whatsInside gift =
    match gift with
    | Book _ ->
        "A book"
    | Chocolate _ ->
        "Some chocolate"
    | Wrapped (innerGift, _) ->
        whatsInside innerGift
    | Boxed innerGift ->
        whatsInside innerGift
    | WithACard (innerGift, _) ->
        whatsInside innerGift

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

let descriptionUsingCata gift =
    let fBook (book:Book) =
        sprintf "'%s'" book.title
    let fChocolate (choc:Chocolate) =
        sprintf "%A chocolate" choc
    let fWrapped (innerText, style) =
        sprintf "%s wrapped in %A paper" innerText style
    let fBox innerText =
        sprintf "%s in a box" innerText
    let fCard (innerText, message) =
        sprintf "%s with a card saying '%s'" innerText message

    cataGift fBook fChocolate fWrapped fBox fCard 

let handleContents fBook fChocolate gift =
    let fWrapped (innerGiftResult, style) =
        innerGiftResult
    let fBox innerGiftResult =
        innerGiftResult
    let fCard (innerGiftResult, message) =
        innerGiftResult

    cataGift fBook fChocolate fWrapped fBox fCard 

type GiftMinusChocolate =
    | Book of Book
    | Apology of string
    | Wrapped of GiftMinusChocolate * WrappingPaperStyle

let removeChocolate gift =
    let fBook (book:Book) =
        Book book
    let fChocolate (choc:Chocolate) =
        Apology "sorry I ate your chocolate"
    let fWrapped (innerGiftResult, style) =
        Wrapped (innerGiftResult, style)
    let fBox innerGiftResult =
        innerGiftResult
    let fCard (innerGiftResult, message) =
        innerGiftResult

    cataGift fBook fChocolate fWrapped fBox fCard gift

let deepCopy gift =
    let fBook = Gift.Book
    let fChocolate = Gift.Chocolate
    let fWrapped = Gift.Wrapped
    let fBox = Gift.Boxed
    let fCard = Gift.WithACard

    cataGift fBook fChocolate fWrapped fBox fCard gift

let upgradeChocolate gift =
    let fBook = Gift.Book
    let fChocolate (choc:Chocolate) =
        Gift.Chocolate { choc with chocType = SeventyPercent }
    let fWrapped = Gift.Wrapped
    let fBox = Gift.Boxed
    let fCard = Gift.WithACard

    cataGift fBook fChocolate fWrapped fBox fCard gift

let wolfHall = { title = "Wolf Hall"; price = 20m }

let yummyChoc = { chocType = SeventyPercent; price = 5m }

let birthdayPresent = WithACard (Gift.Wrapped (Gift.Book wolfHall, HappyBirthday), "Happy Birthday")

let christmasPresent = Gift.Wrapped (Gift.Boxed (Gift.Chocolate yummyChoc), HappyHolidays)

birthdayPresent |> description
christmasPresent |> description

birthdayPresent |> totalCost
christmasPresent |> totalCost

birthdayPresent |> whatsInside
christmasPresent |> whatsInside

birthdayPresent |> totalCostUsingCata
christmasPresent |> totalCostUsingCata

birthdayPresent |> descriptionUsingCata
christmasPresent |> descriptionUsingCata

birthdayPresent
|> handleContents
    (fun book -> "The book you wanted for your birthday")
    (fun choc -> "Your fave chocolate")

christmasPresent
|> handleContents
    (fun book -> "The book you wanted for Christmas")
    (fun choc -> "Don't eat too much over the holidays!")

birthdayPresent |> removeChocolate
christmasPresent |> removeChocolate
christmasPresent |> deepCopy

let cheapChoc = Boxed (Chocolate { chocType = Milk; price = 5m })
cheapChoc |> upgradeChocolate