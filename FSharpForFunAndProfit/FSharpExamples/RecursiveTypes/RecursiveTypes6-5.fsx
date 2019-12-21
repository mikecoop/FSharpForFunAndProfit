#load "RecursiveTypes6-1.fsx"
#load "RecursiveTypes6-4.fsx"

open ``RecursiveTypes6-1``
open ``RecursiveTypes6-4``
open System.Runtime.Serialization.Json

let fromJson<'a> str =
    let serializer = new DataContractJsonSerializer(typeof<'a>)
    let encoding = System.Text.UTF8Encoding()
    use stream = new System.IO.MemoryStream(encoding.GetBytes(s = str))
    let obj = serializer.ReadObject(stream)
    obj :?> 'a

let rec dtoToTree (treeDto:TreeDto<'Leaf, 'Node>) : Tree<'Leaf,'Node> =
    let nullLeaf = Unchecked.defaultof<'Leaf>
    let nullNode = Unchecked.defaultof<'Node>

    if treeDto.NodeData <> nullNode then
        if treeDto.SubTrees = null then
            failwith "subtrees must not be null if node data present"
        else
            let subtrees = treeDto.SubTrees |> Array.map dtoToTree
            InternalNode (treeDto.NodeData, subtrees)
    elif treeDto.LeafData <> nullLeaf then
        LeafNode (treeDto.LeafData)
    else
        failwith "expecting leaf or node data"

let strToBookTitle str =
    match str with
    | null -> failwith "BookTitle must not be null"
    | _ -> str

let strToChocolateType str =
    match str with
    | "Dark" -> Dark
    | "Milk" -> Milk
    | "SeventyPercent" -> SeventyPercent
    | _ -> failwithf "ChocolateType %s not recognized" str

let strToWrappingPaperStyle str =
    match str with
    | "HappyBirthday" -> HappyBirthday
    | "HappyHolidays" -> HappyHolidays
    | "SolidColor" -> SolidColor
    | _ -> failwithf "WrappingPaperStyle %s not recognized" str

let strToCardMessage str =
    match str with
    | null -> failwith "CardMessage must not be null"
    | _ -> str

let bookFromDto (dto:GiftContentsDto) =
    let bookTitle = strToBookTitle dto.BookTitle
    Book { Title = bookTitle; Price = dto.Price }

let chocolateFromDto (dto:GiftContentsDto) =
    let chocType = strToChocolateType dto.ChocolateType
    Chocolate { ChocType = chocType; Price = dto.Price }

let wrappedFromDto (dto:GiftDecorationDto) =
    let wrappingPaperStyle = strToWrappingPaperStyle dto.WrappingPaperStyle
    Wrapped wrappingPaperStyle

let boxedFromDto (dto:GiftDecorationDto) =
    Boxed

let withACardFromDto (dto:GiftDecorationDto) =
    let message = strToCardMessage dto.Message
    WithACard message

let dtoToGift (giftDto:GiftDto) : Gift =
    let fLeaf (leafDto:GiftContentsDto) =
        match leafDto.Discriminator with
        | "Book" -> bookFromDto leafDto
        | "Chocolate" -> chocolateFromDto leafDto
        | _ -> failwithf "Unknown leaf discriminator '%s'" leafDto.Discriminator

    let fNode (nodeDto:GiftDecorationDto) =
        match nodeDto.Discriminator with
        | "Wrapped" -> wrappedFromDto nodeDto
        | "Boxed" -> boxedFromDto nodeDto
        | "WithACard" -> withACardFromDto nodeDto
        | _ -> failwithf "Unknown node discriminator '%s'" nodeDto.Discriminator

    Tree.map fLeaf fNode giftDto

let goodGift = goodJson |> fromJson |> dtoToTree |> dtoToGift

goodGift |> description