open Argu
open MongoDB.Driver

type CLIArguments =
    | [<Mandatory>] Connection_String of string
    interface IArgParserTemplate with
        member s.Usage =
            match s with
            | Connection_String _ -> "specify a connection string"

let optionParser = ArgumentParser.Create<CLIArguments>(programName = "MongoCleanup")

let run (options: ParseResults<CLIArguments>): unit =
    let conn = MongoClient (options.GetResult Connection_String)
    let db = conn.GetDatabase "teambank_konto_service"

    db.ListCollectionNames().ToList()
    |> printfn "%A"

    ()

[<EntryPoint>]
let main argv =
    try
        let options = optionParser.ParseCommandLine(inputs = argv, raiseOnUsage= true)
        run options
    with
    | e -> printfn "%s" e.Message

    0 // return an integer exit code
