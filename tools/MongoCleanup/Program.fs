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

let mongoConnString (cloakConfig: CloakConfig): string =
    let options = cloakConfig.parameters
    let port = options.port |> Option.defaultValue 27017

    let userpass =
        match (options.username, options.password) with
        | (None, None) -> ""
        | (Some(user), None) -> sprintf "%s@" user
        | (Some(user), Some(pass)) -> sprintf "%s:%s@" user pass
        | (None, Some(pass)) -> sprintf ":%s@" pass

    sprintf "mongodb://%s%s:%i" userpass options.hostname port

let run (options: ParseResults<CLIArguments>): unit =
    use stream = new StreamReader(options.GetResult Cloak_Config)

    let config = stream.ReadToEnd () |> Json.deserialize<CloakConfig>
    let connString = mongoConnString config
    printfn "%A" connString
    let conn = MongoClient connString
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
