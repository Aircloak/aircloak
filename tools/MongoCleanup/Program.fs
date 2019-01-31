open Argu
open System

type CLIArguments =
    | [<Mandatory>] Connection_String of string
    interface IArgParserTemplate with
        member s.Usage =
            match s with
            | Connection_String _ -> "specify a connection string"

let optionParser = ArgumentParser.Create<CLIArguments>(programName = "MongoCleanup")

[<EntryPoint>]
let main argv =
    try
        let options = optionParser.ParseCommandLine(inputs = argv, raiseOnUsage= true)
        printfn "%A" options
    with
    | e -> printfn "%s" e.Message

    0 // return an integer exit code
