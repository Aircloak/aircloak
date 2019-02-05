open Argu
open MongoDB.Driver
open MongoDB.Bson
open System.IO
open FSharp.Json
open System.Security.Cryptography

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
      projection : Projection option
      query : string option }

type CloakConfig =
    { parameters : ConnectionProperties
      tables : Map<string, CloakTable> }

type BsonUpdate = BsonValue -> BsonValue

let optionParser =
    ArgumentParser.Create<CLIArguments>(programName = "MongoCleanup")

let rec update' (document: BsonDocument) (keys: string list) (f: BsonUpdate) =
    match keys with
    | [] -> ()
    | [key] ->
        if document.Contains(key) then
            let result = document.GetValue key |> f
            document.Remove(key)
            document.Add(key, result) |> ignore
    | key :: rest ->
        if document.Contains(key) then
            let nested = document.GetValue(key).AsBsonDocument
            update' nested rest f

let update (document: BsonDocument) (key: string) (f: BsonUpdate): unit =
    let keys = key.Split [| '.' |] |> Array.toList
    update' document keys f

let textToInteger (value: BsonValue): BsonValue =
    match System.Int64.TryParse value.AsString with
    | true, num -> BsonInt64(num).AsBsonValue
    | _ -> BsonNull.Value.AsBsonValue

let textToReal (value: BsonValue): BsonValue =
    match System.Double.TryParse value.AsString with
    | true, num -> BsonDouble(num).AsBsonValue
    | _ -> BsonNull.Value.AsBsonValue

let textToDate (value: BsonValue): BsonValue =
    match System.DateTime.TryParse value.AsString with
    | true, date -> BsonDateTime(date).AsBsonValue
    | _ -> BsonNull.Value.AsBsonValue

let convertToBytes (value: BsonValue): byte[] =
    if value.IsBsonBinaryData
    then value.AsByteArray
    else System.Text.Encoding.ASCII.GetBytes value.AsString

let decodeAES (key: string option) (value: BsonValue): BsonValue =
    match key with
    | None -> failwith "No key for aes_cbc_128"
    | Some key ->
        use alg = new RijndaelManaged ()
        alg.Key <- System.Text.Encoding.ASCII.GetBytes key
        alg.IV <- List.replicate 16 0uy |> List.toArray<byte>

        let decryptor = alg.CreateDecryptor(alg.Key, alg.IV)

        try
            use memoryStream = new MemoryStream(convertToBytes value)
            use cryptoStream = new CryptoStream(memoryStream, decryptor, CryptoStreamMode.Read)
            use reader = new StreamReader(cryptoStream)

            let plaintext = reader.ReadToEnd()
            BsonString(plaintext).AsBsonValue
        with
        | _ -> BsonNull.Value.AsBsonValue

let applyDecoder (document : BsonDocument) (decoder : Decoder) : unit =
    let applyDecoder' document decoder =
        for column in decoder.columns do
            match decoder.method with
            | "text_to_integer" -> update document column textToInteger
            | "text_to_real" -> update document column textToReal
            | "text_to_date" -> update document column textToDate
            | "text_to_datetime" -> update document column textToDate
            | "aes_cbc_128" -> update document column (decodeAES decoder.key)
            // | "real_to_integer"
            // | "text_to_boolean"
            // | "real_to_boolean"
            // | "base64"
            // | "substring"
            | _ -> ()

    try
        applyDecoder' document decoder
    with
    | e -> printfn "Error %A when applying %A" e.Message decoder

let applyDecoders (decoders : Decoder list) (document : BsonDocument) : unit =
    for decoder in decoders do
        applyDecoder document decoder

let readCollection (db: IMongoDatabase) (name: string) : System.Collections.Generic.List<BsonDocument> =
    db.GetCollection<BsonDocument>(name).Find(fun _ -> true).ToList()

let writeCollection (db: IMongoDatabase) (name: string) (documents: seq<BsonDocument>): unit =
    let collection = db.GetCollection<BsonDocument>(name)
    for document in documents do
        let field = StringFieldDefinition<BsonDocument, BsonValue>("_id")
        let filter = Builders<BsonDocument>.Filter.Eq(field, document.GetValue("_id"))
        collection.ReplaceOne(filter, document) |> ignore

let decode (documents: seq<BsonDocument>) (decoders : Decoder list) : unit =
    for document in documents do
        applyDecoders decoders document

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

let project (tables: Map<string, CloakTable>) (db: IMongoDatabase) : unit =
    ()

let run (options : ParseResults<CLIArguments>) : unit =
    use stream = new StreamReader(options.GetResult Cloak_Config)
    let jsonConfig = JsonConfig.create (jsonFieldNaming = Json.snakeCase)
    let config =
        stream.ReadToEnd() |> Json.deserializeEx<CloakConfig> jsonConfig
    let conn = config |> mongoConnString |> MongoClient
    let db = conn.GetDatabase config.parameters.database

    let data = config.tables |> Map.map (fun k _ -> readCollection db k)

    for KeyValue(k, v) in config.tables |> Seq.filter (fun kv -> Option.isSome kv.Value.decoders) do
        decode (data.Item k) v.decoders.Value

    project config.tables db

    for KeyValue(k, v) in config.tables |> Seq.filter (fun kv -> Option.isSome kv.Value.decoders || Option.isSome kv.Value.projection) do
        writeCollection db k (data.Item k)

    ()

[<EntryPoint>]
let main argv =
    try
        let options =
            optionParser.ParseCommandLine(inputs = argv, raiseOnUsage = true)
        run options
    with e -> printfn "%s" e.Message
    0 // return an integer exit code
