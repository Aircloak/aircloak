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

let update (document: BsonDocument) (key: string) (f: (BsonValue -> BsonValue)): unit =
    if document.Contains(key) then
        let result = document.GetValue key |> f
        document.Remove(key)
        document.Add(key, result) |> ignore

let textToInteger (value: BsonValue): BsonValue =
    match System.Int64.TryParse value.AsString with
    | true, num -> BsonInt64(num).AsBsonValue
    | _ -> BsonNull.Value.AsBsonValue

let applyDecoder (document : BsonDocument) (decoder : Decoder) : unit =
    for column in decoder.columns do
        match decoder.method with
        | "text_to_integer" -> update document column textToInteger
        | _ -> ()

let applyDecoders (decoders : Decoder list) (document : BsonDocument) : unit =
    for decoder in decoders do
        applyDecoder document decoder

let decode (table : string) (decoders : Decoder list) (db : IMongoDatabase) : unit =
    let collection = db.GetCollection<BsonDocument>(table)
    let mutable documents = collection.Find(fun _ -> true).ToList()

    for document in documents do
        applyDecoders decoders document |> ignore
        let field = StringFieldDefinition<BsonDocument, BsonValue>("_id")
        let filter = Builders<BsonDocument>.Filter.Eq(field, document.GetValue("_id"))
        collection.ReplaceOne(filter, document) |> ignore

    documents
    |> Seq.map (fun x -> x.ToJson ())
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
