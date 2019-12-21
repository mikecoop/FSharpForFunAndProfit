
type FileSystemItem =
    | File of File
    | Directory of Directory
and File = { name: string; fileSize: int }
and Directory = { name: string; dirSize: int; subItems: FileSystemItem list }

let readme = File { name = "readme.txt"; fileSize = 1 }
let config = File { name = "config.xml"; fileSize = 2 }
let build  = File { name = "build.bat"; fileSize = 3 }
let src = Directory { name = "src"; dirSize = 10; subItems = [ readme; config; build ] }
let bin = Directory { name = "bin"; dirSize = 10; subItems = [ ] }
let root = Directory { name = "root"; dirSize = 5; subItems = [ src; bin ] }

let rec foldFS fFile fDir acc item : 'r =
    let recurse = foldFS fFile fDir
    match item with
    | File file ->
        fFile acc file
    | Directory dir ->
        let newAcc = fDir acc (dir.name, dir.dirSize)
        dir.subItems |> List.fold recurse newAcc

let totalSize fileSystemItem =
    let fFile acc (file:File) =
        acc + file.fileSize
    let fDir acc (name, size) =
        acc + size
    foldFS fFile fDir 0 fileSystemItem

readme |> totalSize
src |> totalSize
root |> totalSize

let largestFile fileSystemItem =
    let fFile (largestSoFarOpt:File option) (file:File) =
        match largestSoFarOpt with
        | None ->
            Some file
        | Some largestSoFar ->
            if largestSoFar.fileSize > file.fileSize then
                Some largestSoFar
            else
                Some file
    let fDir largestSoFarOpt (name, size) =
        largestSoFarOpt

    foldFS fFile fDir None fileSystemItem

readme |> largestFile
src |> largestFile
bin |> largestFile
root |> largestFile

let rec firstSumBiggerThan100 sumSoFar listOfInts =
    match listOfInts with
    | [ ] ->
        sumSoFar
    | head :: tail ->
        let newSumSoFar = head + sumSoFar
        if newSumSoFar > 100 then
            newSumSoFar
        else
            firstSumBiggerThan100 newSumSoFar tail

[ 30; 40; 50; 60 ] |> firstSumBiggerThan100 0
[ 1 .. 3 .. 100 ] |> firstSumBiggerThan100 0