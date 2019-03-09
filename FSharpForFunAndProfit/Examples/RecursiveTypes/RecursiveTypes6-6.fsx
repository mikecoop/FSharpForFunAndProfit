#load "RecursiveTypes6-1.fsx"
#load "RecursiveTypes6-4.fsx"

open ``RecursiveTypes6-1``
open ``RecursiveTypes6-4``
open System.Runtime.Serialization.Json

type TheResult<'a> = 
    | Success of 'a
    | Failure of string list

module TheResult =

    let retn x =  
        Success x

    let failWithMsg msg =  
        Failure [msg]
    
    let bind f xR =    
        match xR with
        | Success x -> f x
        | Failure errs -> Failure errs

    let map f xR =    
        match xR with
        | Success x -> Success (f x)
        | Failure errs -> Failure errs

    let apply fR xR =    
        match fR,xR with
        | Success f, Success x -> Success (f x)
        | Failure errs, Success x -> Failure errs
        | Success f, Failure errs -> Failure errs
        | Failure errs1, Failure errs2 -> Failure (errs1 @ errs2)

    let lift2 f x y = 
        let (<!>) = map
        let (<*>) = apply 
        f <!> x <*> y

    let lift3 f x y z = 
        let (<!>) = map
        let (<*>) = apply 
        f <!> x <*> y <*> z

    /// Convert a list of Results into a Result of list
    let sequenceList listOfResult = 
        // from the lower level
        let (<*>) = apply 

        // from the traversable level
        let cons head tail = head :: tail

        // do the traverse
        let folder head tail = 
            retn cons <*> head <*> tail 
        List.foldBack folder listOfResult (retn [])

    /// Convert a seq of Results into a Result of seq
    let sequenceSeq seqOfResult = 
        seqOfResult 
        |> List.ofSeq 
        |> sequenceList 
        |> map (List.toSeq)

let fromJson<'a> str =
    try
        let serializer = new DataContractJsonSerializer(typeof<'a>)
        let encoding = System.Text.UTF8Encoding()
        use stream = new System.IO.MemoryStream(encoding.GetBytes(s = str))
        let obj = serializer.ReadObject(stream)
        obj :?> 'a
        |> TheResult.Success
    with
    | ex ->
        TheResult.Failure [ ex.Message ]

let sequenceTreeOfResult tree =
    let (<*>) = TheResult.apply
    let retn = TheResult.retn

    let fLeaf data = 
        retn LeafNode <*> data

    let fNode data subitems = 
        let makeNode data items = InternalNode (data, items)
        let subItems = TheResult.sequenceSeq subitems 
        retn makeNode <*> data <*> subItems

    // do the traverse
    Tree.cata fLeaf fNode tree

let rec dtoToTreeOfResults (treeDto:TreeDto<'Leaf, 'Node>) : Tree<TheResult<'Leaf>, TheResult<'Node>> =
    let nullLeaf = Unchecked.defaultof<'Leaf>
    let nullNode = Unchecked.defaultof<'Node>
    
    // check if there is nodeData present
    if treeDto.NodeData <> nullNode then
        if treeDto.SubTrees = null then
            LeafNode <| TheResult.failWithMsg "subtrees must not be null if node data present"
        else
            let subtrees = treeDto.SubTrees |> Array.map dtoToTreeOfResults 
            InternalNode (TheResult.retn treeDto.NodeData,subtrees) 
    // check if there is leafData present
    elif treeDto.LeafData <> nullLeaf then
        LeafNode <| TheResult.retn (treeDto.LeafData) 
    // if both missing then fail
    else
        LeafNode <| TheResult.failWithMsg "expecting leaf or node data"

let dtoToTree treeDto =
    treeDto |> dtoToTreeOfResults |> sequenceTreeOfResult

let strToBookTitle str =
    match str with
    | null -> TheResult.Failure [ "BookTitle must not be null" ]
    | _ -> TheResult.Success str

let strToChocolateType str =
    match str with
    | "Dark" -> TheResult.Success Dark
    | "Milk" -> TheResult.Success Milk
    | "SeventyPercent" -> TheResult.Success  SeventyPercent
    | _ -> TheResult.Failure [ sprintf "ChocolateType %s not recognized" str ]

let strToWrappingPaperStyle str =
    match str with
    | "HappyBirthday" -> TheResult.Success HappyBirthday
    | "HappyHolidays" -> TheResult.Success HappyHolidays
    | "SolidColor" -> TheResult.Success SolidColor
    | _ -> TheResult.Failure [ sprintf "WrappingPaperStyle %s not recognized" str ]

let strToCardMessage str =
    match str with
    | null -> TheResult.Failure [ "CardMessage must not be null" ]
    | _ -> TheResult.Success str

let bookFromDto (dto:GiftContentsDto) =
    let book bookTitle price = 
        Book { Title = bookTitle; Price = price }

    let bookTitle = strToBookTitle dto.BookTitle
    let price = TheResult.Success dto.Price
    TheResult.lift2 book bookTitle price 

let chocolateFromDto (dto:GiftContentsDto) =
    let choc chocType price = 
        Chocolate { ChocType = chocType; Price = price }

    let chocType = strToChocolateType dto.ChocolateType 
    let price = TheResult.Success dto.Price
    TheResult.lift2 choc chocType price 

let wrappedFromDto (dto:GiftDecorationDto) =
    let wrappingPaperStyle = strToWrappingPaperStyle dto.WrappingPaperStyle
    TheResult.map Wrapped wrappingPaperStyle 

let boxedFromDto (dto:GiftDecorationDto) =
    TheResult.Success Boxed

let withACardFromDto (dto:GiftDecorationDto) =
    let message = strToCardMessage dto.Message
    TheResult.map WithACard message 

let dtoToGift (giftDto:GiftDto) : TheResult<Gift> =
    let fLeaf (leafDto:GiftContentsDto) =
        match leafDto.Discriminator with
        | "Book" -> bookFromDto leafDto
        | "Chocolate" -> chocolateFromDto leafDto
        | _ -> TheResult.Failure [ sprintf "Unknown leaf discriminator '%s'" leafDto.Discriminator ]

    let fNode (nodeDto:GiftDecorationDto) =
        match nodeDto.Discriminator with
        | "Wrapped" -> wrappedFromDto nodeDto
        | "Boxed" -> boxedFromDto nodeDto
        | "WithACard" -> withACardFromDto nodeDto
        | _ -> TheResult.Failure [ sprintf "Unknown node discriminator '%s'" nodeDto.Discriminator ]

    Tree.map fLeaf fNode giftDto |> sequenceTreeOfResult

let goodGift = goodJson |> fromJson |> TheResult.bind dtoToTree |> TheResult.bind dtoToGift

goodGift |> TheResult.map description

let badJson1 = goodJson.Replace("LeafData","LeafDataXX")
let badJson1_result = badJson1 |> fromJson |> TheResult.bind dtoToTree |> TheResult.bind dtoToGift

let badJson2 = goodJson.Replace("Wrapped","Wrapped2")
let badJson2_result = badJson2 |> fromJson |> TheResult.bind dtoToTree |> TheResult.bind dtoToGift

let badJson4 = goodJson.Replace("HappyHolidays","HappyHolidays2")
                       .Replace("SeventyPercent","SeventyPercent2")
let badJson4_result = badJson4 |> fromJson |> TheResult.bind dtoToTree |> TheResult.bind dtoToGift