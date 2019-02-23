
type FileSystemItem =
    | File of File
    | Directory of Directory
and File = { name: string; fileSize: int }
and Directory = { name: string; dirSize: int; subItems: FileSystemItem list }

let readme = File { name = "readme.txt"; fileSize = 1 }
let config = File { name = "config.xml"; fileSize = 2 }
let build = File { name = "build.bat"; fileSize = 3 }
let src = Directory { name = "src"; dirSize = 10; subItems = [ readme; config; build ] }
let bin = Directory { name = "bin"; dirSize = 10; subItems = [ ] }
let root = Directory { name = "root"; dirSize = 5; subItems = [ src; bin ] }

let rec cataFS fFile fDir item : 'r =
    let recurse = cataFS fFile fDir
    match item with
    | File file ->
        fFile file
    | Directory dir ->
        let listOfRs = dir.subItems |> List.map recurse
        fDir (dir.name, dir.dirSize, listOfRs)

let totalSize fileSystemItem =
    let fFile (file:File) =
        file.fileSize
    let fDir (name, size, subSizes) =
        (List.sum subSizes) + size
    cataFS fFile fDir fileSystemItem

readme |> totalSize
src |> totalSize
root |> totalSize

let largestFile fileSystemItem =
    let ifNone deflt opt =
        defaultArg opt deflt

    let fileSize fileOpt =
        fileOpt
        |> Option.map (fun file -> file.fileSize)
        |> ifNone 0

    let fFile (file:File) =
        Some file

    let fDir (name, size, subFiles) =
        match subFiles with 
        | [] ->
            None
        | subFiles ->
            subFiles
            |> List.maxBy fileSize

    cataFS fFile fDir fileSystemItem

readme |> largestFile
src |> largestFile
bin |> largestFile
root |> largestFile

type Product =
    | Bought of BoughtProduct
    | Made of MadeProduct
and BoughtProduct =
    { name: string
      weight: int
      vendor: string option }
and MadeProduct =
    { name: string
      weight: int
      components: Component list }
and Component =
    { qty: int
      product: Product }

let label =
    Bought { name = "label"; weight = 1; vendor = Some "ACME" }
let bottle =
    Bought { name = "bottle"; weight = 2; vendor = Some "ACME" }
let formulation =
    Bought { name = "formulation"; weight = 3; vendor = None }

let shampoo =
    Made { name = "shampoo"; weight = 10; components =
    [ { qty = 1; product = formulation }
      { qty = 1; product = bottle }
      { qty = 2; product = label } ] }

let twoPack =
    Made { name = "twoPack"; weight = 5; components =
    [ { qty = 2; product = shampoo } ] }

let rec cataProduct fBought fMade product : 'r =
    let recurse = cataProduct fBought fMade

    let converComponentToTuple comp =
        (comp.qty, recurse comp.product)

    match product with
    | Bought bought ->
        fBought bought
    | Made made ->
        let componentTuples =
            made.components
            |> List.map converComponentToTuple
        fMade (made.name, made.weight, componentTuples)

let productWeight product =
    let fBought (bought:BoughtProduct) =
        bought.weight

    let fMade (name, weight, componentTuples) =
        let componentWeight (qty, weight) =
            qty * weight
        let totalComponentWeight =
            componentTuples
            |> List.sumBy componentWeight
        totalComponentWeight + weight
    
    cataProduct fBought fMade product

label |> productWeight
shampoo |> productWeight
twoPack |> productWeight

type VendorScore = { vendor: string; score: int }

let vendor vs = vs.vendor
let score vs = vs.score

let mostUsedVendor product =
    
    let fBought (bought:BoughtProduct) =
        bought.vendor
        |> Option.map (fun vendor -> { vendor = vendor; score = 1 })
        |> Option.toList

    let fMade (name, weight, subResults) =
        let totalScore (vendor, vendorScores) =
            let totalScore = vendorScores |> List.sumBy score
            { vendor = vendor; score = totalScore }

        subResults
        |> List.collect snd
        |> List.groupBy vendor
        |> List.map totalScore

    cataProduct fBought fMade product
    |> List.sortByDescending score
    |> List.tryHead

label |> mostUsedVendor
formulation |> mostUsedVendor
shampoo |> mostUsedVendor
twoPack |> mostUsedVendor