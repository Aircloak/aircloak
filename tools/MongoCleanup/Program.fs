open Argu
open MongoDB.Driver
open MongoDB.Bson
open System.IO
open FSharp.Json

type CLIArguments =
    | [<Mandatory>] Cloak_Config of path : string
    interface IArgParserTemplate with
        member s.Usage =
            match s with
            | Cloak_Config _ ->
                "specify the cloak config describing the data source"

type ConnectionProperties =
    { hostname : string
      database : string
      port : int option
      username : string option
      password : string option }

type Projection =
    { table : string
      foreignKey : string
      primaryKey : string }

type Decoder =
    { method : string
      key : string option
      columns : string list }

type CloakTable =
    { userId : string option
      decoders : Decoder list option
      projection : Projection option }

type CloakConfig =
    { parameters : ConnectionProperties
      tables : Map<string, CloakTable> }

let optionParser =
    ArgumentParser.Create<CLIArguments>(programName = "MongoCleanup")
let applyDecoder (decode : Decoder) (document : BsonDocument) : BsonDocument =
    BsonDocument
let applyDecoders (decoders : Decoder list) (document : BsonDocument) : BsonDocument =
    Seq.fold applyDecoder document decoders

let decode (table : string) (decoders : Decoder list) (db : IMongoDatabase) : unit =
    db.GetCollection<BsonDocument>(table).Find(fun _ -> true).ToList()
    |> Seq.map (applyDecoders decoders)
    |> printfn "%A"
    ()

let mongoConnString (cloakConfig : CloakConfig) : string =
    let options = cloakConfig.parameters
    let port = options.port |> Option.defaultValue 27017

    let userpass =
        match (options.username, options.password) with
        | (None, None) -> ""
        | (Some(user), None) -> sprintf "%s@" user
        | (Some(user), Some(pass)) -> sprintf "%s:%s@" user pass
        | (None, Some(pass)) -> sprintf ":%s@" pass
    sprintf "mongodb://%s%s:%i" userpass options.hostname port

let run (options : ParseResults<CLIArguments>) : unit =
    use stream = new StreamReader(options.GetResult Cloak_Config)
    let jsonConfig = JsonConfig.create (jsonFieldNaming = Json.snakeCase)
    let config =
        stream.ReadToEnd() |> Json.deserializeEx<CloakConfig> jsonConfig
    let connString = mongoConnString config
    let conn = MongoClient connString
    let db = conn.GetDatabase config.parameters.database
    for kv in config.tables
              |> Seq.filter (fun kv -> Option.isSome kv.Value.decoders) do
        decode kv.Key kv.Value.decoders.Value db
    ()

[<EntryPoint>]
let main argv =
    try
        let options =
            optionParser.ParseCommandLine(inputs = argv, raiseOnUsage = true)
        run options
    with e -> printfn "%s" e.Message
    0 // return an integer exit code
