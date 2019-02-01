open Argu
open MongoDB.Driver
open System.IO
open FSharp.Json

type CLIArguments =
    | [<Mandatory>] Cloak_Config of path:string
    interface IArgParserTemplate with
        member s.Usage =
            match s with
            | Cloak_Config _ -> "specify the cloak config describing the data source"

type ConnectionProperties = {
    hostname: string
    database: string
    port: int option
    username: string option
    password: string option
}

type CloakConfig = {
    parameters: ConnectionProperties
}

let optionParser = ArgumentParser.Create<CLIArguments>(programName = "MongoCleanup")

let buildMongoSettings (cloakConfig: CloakConfig): MongoClientSettings =
    let options = cloakConfig.parameters

    MongoClientSettings(
        Server = MongoServerAddress(options.hostname, options.port |> Option.defaultValue 27017)
    )

let run (options: ParseResults<CLIArguments>): unit =
    use stream = new StreamReader(options.GetResult Cloak_Config)

    let config = stream.ReadToEnd () |> Json.deserialize<CloakConfig>
    let mongoSettings = buildMongoSettings config
    let conn = MongoClient mongoSettings
    let db = conn.GetDatabase config.parameters.database

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
