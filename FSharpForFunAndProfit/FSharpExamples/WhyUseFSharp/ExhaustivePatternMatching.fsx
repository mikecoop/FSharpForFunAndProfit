
type State = New | Draft | Published | Inactive | Discontinued
let handleState state =
    match state with
    | Inactive -> "Inactive"
    | Draft -> "Draft"
    | New -> "New"
    | Published -> "Published"

let getFileInfo filePath =
    let fi = new System.IO.FileInfo(filePath)
    if fi.Exists then Some (fi) else None

let rec movingAverages list =
    match list with
    | [ ] -> [ ]
    | x :: y :: rest -> 
        let avg = (x + y) / 2.0
        avg :: movingAverages(y :: rest)
    | [ _ ] -> [ ]

movingAverages [ 1.0 ]
movingAverages [ 1.0; 2.0 ]
movingAverages [ 1.0; 2.0; 3.0 ]

type FileErrorReason =
    | FileNotFound of string
    | UnauthorizedAccess of string * System.Exception

let performActionOnFile action filePath =
    try
        use sr = new System.IO.StreamReader(filePath: string)
        let result = action sr
        sr.Close()
        Ok (result)
    with
    | :? System.IO.FileNotFoundException as ex -> Error (FileNotFound filePath)
    | :? System.Security.SecurityException as ex -> Error (UnauthorizedAccess (filePath, ex))

let middleLayerDo action filePath =
    let fileResult = performActionOnFile action filePath
    fileResult

let topLayerDo action filePath =
    let fileResult = middleLayerDo action filePath
    fileResult

let printFirstLineOfFile filePath =
    let fileResult = topLayerDo (fun fs -> fs.ReadLine()) filePath

    match fileResult with
    | Ok result -> printfn "first line is %s" result
    | Error reason -> 
        match reason with
        | FileNotFound file -> printfn "File not found: %s" file
        | UnauthorizedAccess (file, _) -> printfn "You do not have access to the file: %s" file