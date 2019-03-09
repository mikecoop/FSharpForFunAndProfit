#load "RecursiveTypes6-1.fsx"

open ``RecursiveTypes6-1``
open System.IO

let foldLinesAsync folder acc (fi:FileInfo) =
    async {
        let mutable acc = acc
        let mutable lineNo = 1
        use sr = new StreamReader (path = fi.FullName)
        while not sr.EndOfStream do
            let! lineText = sr.ReadLineAsync() |> Async.AwaitTask
            acc <- folder acc lineNo lineText
            lineNo <- lineNo + 1
        return acc }

let asyncMap f asyncX = async {
    let! x = asyncX
    return (f x) }

let matchPattern textPattern (fi:FileInfo) =
    let regex = System.Text.RegularExpressions.Regex(pattern = textPattern)

    let folder results lineNo lineText =
        if regex.IsMatch lineText then
            let result = sprintf "%40s:%-5i %s" fi.Name lineNo lineText
            result :: results
        else
            results

    fi
    |> foldLinesAsync folder [ ]
    |> asyncMap List.rev

let grep filePattern textPattern fileSystemItem =
    let regex = System.Text.RegularExpressions.Regex(pattern = filePattern)

    let matchFile (fi:FileInfo) =
        if regex.IsMatch fi.Name then
            Some (matchPattern textPattern fi)
        else
            None

    let fFile asyncs (fi:FileInfo) =
        (matchFile fi) :: asyncs

    let fDir asyncs (di:DirectoryInfo) =
        asyncs

    fileSystemItem
    |> Tree.fold fFile fDir [ ]
    |> Seq.choose id
    |> Async.Parallel
    |> asyncMap (Array.toList >> List.collect id)

currentDir
|> grep "fsx" "LinkedList"
|> Async.RunSynchronously