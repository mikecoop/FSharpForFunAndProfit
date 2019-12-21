
type Tree<'LeafData, 'INodeData> =
    | LeafNode of 'LeafData
    | InternalNode of 'INodeData * Tree<'LeafData, 'INodeData> seq

type FileInfo = { name: string; fileSize: int }
type DirectoryInfo = { name: string; dirSize: int }

type FileSystemItem = Tree<FileInfo, DirectoryInfo>

module Tree = 
    let rec cata fLeaf fNode (tree:Tree<'LeafData, 'INodeData>) : 'r =
        let recurse = cata fLeaf fNode
        match tree with
        | LeafNode leafInfo ->
            fLeaf leafInfo
        | InternalNode (nodeInfo, subtrees) ->
            fNode nodeInfo (subtrees |> Seq.map recurse)

    let rec fold fLeaf fNode acc (tree:Tree<'LeafData, 'INodeData>) : 'r =
        let recurse = fold fLeaf fNode
        match tree with
        | LeafNode leafInfo ->
            fLeaf acc leafInfo
        | InternalNode (nodeInfo, subtrees) ->
            let localAccum = fNode acc nodeInfo
            let finalAccum = subtrees |> Seq.fold recurse localAccum
            finalAccum

    let rec map fLeaf fNode (tree:Tree<'LeafData, 'INodeData>) =
        let recurse = map fLeaf fNode
        match tree with
        | LeafNode leafInfo ->
            let newLeafInfo = fLeaf leafInfo
            LeafNode newLeafInfo
        | InternalNode (nodeInfo, subtrees) ->
            let newNodeInfo = fNode nodeInfo
            let newSubtrees = subtrees |> Seq.map recurse
            InternalNode (newNodeInfo, newSubtrees)

    let rec iter fLeaf fNode (tree:Tree<'LeafData, 'INodeData>) =
        let recurse = iter fLeaf fNode
        match tree with
        | LeafNode leafInfo ->
            fLeaf leafInfo
        | InternalNode (nodeInfo, subtrees) ->
            subtrees |> Seq.iter recurse
            fNode nodeInfo

let fromFile (fileInfo:FileInfo) =
    LeafNode fileInfo

let fromDir (dirInfo:DirectoryInfo) subitems =
    InternalNode (dirInfo, subitems)

let readme = fromFile { name = "readme.txt"; fileSize = 1 }
let config = fromFile { name = "config.xml"; fileSize = 2 }
let build = fromFile { name = "build.bad"; fileSize = 3 }
let src = fromDir { name = "src"; dirSize = 10 } [ readme; config; build ]
let bin = fromDir { name = "bin"; dirSize = 10 } [ ]
let root = fromDir { name = "root"; dirSize = 5 } [ src; bin ]

let totalSize fileSystemItem =
    let fFile acc (file:FileInfo) = 
        acc + file.fileSize
    let fDir acc (dir:DirectoryInfo) =
        acc + dir.dirSize
    Tree.fold fFile fDir 0 fileSystemItem

readme |> totalSize
src |> totalSize
root |> totalSize

let largestFile fileSystemItem =
    let fFile (largestSoFarOpt:FileInfo option) (file:FileInfo) =
        match largestSoFarOpt with
        | None ->
            Some file
        | Some largestSoFar ->
            if largestSoFar.fileSize > file.fileSize then
                Some largestSoFar
            else
                Some file

    let fDir largestSoFarOpt dirInfo =
        largestSoFarOpt

    Tree.fold fFile fDir None fileSystemItem

readme |> largestFile
src |> largestFile
bin |> largestFile
root |> largestFile

open System
open System.IO

type FileSystemTree = Tree<IO.FileInfo, IO.DirectoryInfo>

let fromFileInfo (fileInfo:FileInfo) =
    LeafNode fileInfo

let rec fromDirInfo (dirInfo:DirectoryInfo) =
    let subItems = seq {
        yield! dirInfo.EnumerateFiles() |> Seq.map fromFileInfo
        yield! dirInfo.EnumerateDirectories() |> Seq.map fromDirInfo }
    InternalNode (dirInfo, subItems)

let totalSizeFileSystem fileSystemItem =
    let fFile acc (file:FileInfo) =
        acc + file.Length
    let fDir acc (dir:DirectoryInfo) =
        acc
    Tree.fold fFile fDir 0L fileSystemItem

Directory.SetCurrentDirectory __SOURCE_DIRECTORY__

let currentDir = fromDirInfo (DirectoryInfo("."))

currentDir |> totalSizeFileSystem

let largestFileFileSystem fileSystemItem =
    let fFile (largestSoFarOpt:FileInfo option) (file:FileInfo) =
        match largestSoFarOpt with
        | None ->
            Some file
        | Some largestSoFar ->
            if largestSoFar.Length > file.Length then
                Some largestSoFar
            else
                Some file

    let fDir largestSoFarOpt dirInfo =
        largestSoFarOpt

    Tree.fold fFile fDir None fileSystemItem

currentDir |> largestFileFileSystem

let dirListing fileSystemItem =
    let printDate (d:DateTime) = d.ToString()
    let mapFile (fi:FileInfo) =
        sprintf "%10i %s %-s" fi.Length (printDate fi.LastWriteTime) fi.Name
    let mapDir (di:DirectoryInfo) =
        di.FullName
    Tree.map mapFile mapDir fileSystemItem

currentDir
|> dirListing
|> Tree.iter (printfn "%s") (printfn "\n%s")