
#load "RecursiveTypes6-1.fsx"

open ``RecursiveTypes6-1``
open System.IO

let nextIdentity =
    let id = ref 0
    fun () ->
        id := !id + 1
        !id

type PrimaryKey =
    | FileId of int
    | DirId of int

let insertDbFile name (fileSize:int64) =
    let id = nextIdentity()
    printfn "%10s: inserting id:%i name:%s size:%i" "DbFile" id name fileSize
    FileId id

let insertDbDir name =
    let id = nextIdentity()
    printfn "%10s: inserting id:%i name:%s" "DbDir" id name
    DirId id

let insertDbDir_File dirId fileId =
    printfn "%10s: inserting parentDir:%i childFile:%i" "DbDir_File" dirId fileId

let insertDbDir_Dir parentDirId childDirId =
    printfn "%10s: inserting parentDir:%i childDir:%i" "DbDir_Dir" parentDirId childDirId

let pkToInt primaryKey =
    match primaryKey with
    | FileId fileId -> fileId
    | DirId dirId -> dirId

let insertFileSystemTree fileSystemItem =
    let fFile (fi:FileInfo) =
        insertDbFile fi.Name fi.Length

    let fDir (di:DirectoryInfo) childIds =
        let dirId = insertDbDir di.Name
        let parentPK = pkToInt dirId
        childIds |> Seq.iter (fun childId ->
            match childId with
            | FileId fileId -> insertDbDir_File parentPK fileId
            | DirId childDirId -> insertDbDir_Dir parentPK childDirId )
        dirId

    fileSystemItem
    |> Tree.cata fFile fDir

let currentDir = fromDirInfo (DirectoryInfo ("."))
currentDir
|> insertFileSystemTree
