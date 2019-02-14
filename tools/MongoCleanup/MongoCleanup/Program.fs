open System
open Argu
open MongoDB.Driver
open MongoDB.Bson
open System.IO
open FSharp.Json
open System.Security.Cryptography

type CLIArguments =
    |  [<Mandatory>] Cloak_Config of path : string
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
      from : int32 option
      [<JsonField("for")>]
      length : int32 option
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

let rec update' (document : BsonDocument) (keys : string list) (f : BsonUpdate) =
    match keys with
    | [] -> ()
    | [ key ] ->
        if document.Contains(key) then
            let result = document.GetValue key |> f
            document.Remove(key)
            document.Add(key, result) |> ignore
    | key :: rest ->
        if document.Contains(key) then
            let nested = document.GetValue(key).AsBsonDocument
            update' nested rest f

let update (document : BsonDocument) (key : string) (f : BsonUpdate) : unit =
    let keys = key.Split [| '.' |] |> Array.toList
    update' document keys f

let bsonNull : BsonValue = upcast BsonNull.Value

let safely<'T when 'T :> BsonValue> (f : unit -> 'T) : BsonValue =
    try
        upcast f()
    with
    | :? InvalidCastException -> bsonNull
    | :? FormatException -> bsonNull

let textToInteger (value : BsonValue) : BsonValue =
    safely (fun () -> value.AsString |> System.Int64.Parse |> BsonInt64)

let textToReal (value : BsonValue) : BsonValue =
    safely (fun () -> value.AsString |> System.Double.Parse |> BsonDouble)

let textToDate (value : BsonValue) : BsonValue =
    safely (fun () -> System.DateTime.Parse(value.AsString) |> BsonDateTime)

let realToInteger (value : BsonValue) : BsonValue =
    safely (fun () -> BsonInt64(int64 value.AsDouble))

let realToBoolean (value : BsonValue) : BsonValue =
    safely (fun () -> BsonBoolean(value.AsDouble <> 0.0))

let textToBoolean (value : BsonValue) : BsonValue =
    safely (fun () -> value.AsString |> System.Boolean.Parse |> BsonBoolean)

let base64 (value : BsonValue) : BsonValue =
    safely (fun () -> value.AsString |> System.Convert.FromBase64String |> BsonBinaryData)

let substring (from : int32) (length : int32) (value : BsonValue) : BsonValue =
    safely (fun () -> value.AsString.Substring(from, length) |> BsonString)

let convertToBytes (value : BsonValue) : byte [] =
    if value.IsBsonBinaryData
    then value.AsByteArray
    else System.Text.Encoding.ASCII.GetBytes value.AsString

let decodeAES (key : string option) (value : BsonValue) : BsonValue =
    match key with
    | None -> failwith "No key for aes_cbc_128"
    | Some key ->
        use alg = new RijndaelManaged()
        alg.Key <- System.Text.Encoding.ASCII.GetBytes key
        alg.IV <- List.replicate 16 0uy |> List.toArray<byte>

        let decryptor = alg.CreateDecryptor(alg.Key, alg.IV)

        try
            use memoryStream = new MemoryStream(convertToBytes value)
            use cryptoStream = new CryptoStream(memoryStream, decryptor, CryptoStreamMode.Read)
            use reader = new StreamReader(cryptoStream)

            let plaintext = reader.ReadToEnd()
            upcast BsonString(plaintext)
        with
        | _ -> bsonNull

let substringDecoder (decoder : Decoder) =
    match decoder.from, decoder.length with
    | Some(from), Some(length) -> substring from length
    | _ -> failwith "The substring decoder requires specifying from and for"

let applyDecoder (document : BsonDocument) (decoder : Decoder) : unit =
    let applyDecoder' document decoder =
        for column in decoder.columns do
            match decoder.method with
            | "text_to_integer" -> update document column textToInteger
            | "text_to_real" -> update document column textToReal
            | "text_to_date" -> update document column textToDate
            | "text_to_datetime" -> update document column textToDate
            | "aes_cbc_128" -> update document column (decodeAES decoder.key)
            | "real_to_integer" -> update document column realToInteger
            | "real_to_boolean" -> update document column realToBoolean
            | "text_to_boolean" -> update document column textToBoolean
            | "base64" -> update document column base64
            | "substring" -> update document column (substringDecoder decoder)
            | _ -> ()

    try
        applyDecoder' document decoder
    with
    | e -> printfn "Error %A when applying %A" e.Message decoder

let applyDecoders (decoders : Decoder list) (document : BsonDocument) : unit =
    for decoder in decoders do
        applyDecoder document decoder

let readCollection (db : IMongoDatabase) (name : string) : seq<BsonDocument> =
    upcast db.GetCollection<BsonDocument>(name).Find(fun _ -> true).ToList()

let writeCollection (db : IMongoDatabase) (name : string) (documents : seq<BsonDocument>) : unit =
    let collection = db.GetCollection<BsonDocument>(name)

    let updates =
        documents
        |> Seq.map (fun doc ->
            let field = StringFieldDefinition<BsonDocument, BsonValue>("_id")
            let filter = Builders<BsonDocument>.Filter.Eq(field, doc.GetValue("_id"))

            ReplaceOneModel<BsonDocument>(filter, doc)
        )
        |> Seq.cast

    collection.BulkWrite(updates) |> ignore

let decode (documents : seq<BsonDocument>) (decoders : Decoder list) : Async<unit> =
    async {
        let! _ =
            documents
            |> Seq.map (fun doc -> async { applyDecoders decoders doc })
            |> Async.Parallel

        ()
    }

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

let acUserId = "_ac_user_id"

let guessUserId (data : seq<BsonDocument>) (proposed : string) : string =
    if Seq.forall (fun (x : BsonDocument) -> not (x.Contains proposed)) data
        then proposed else acUserId

let projectOne (data : Map<string, seq<BsonDocument>>) (config : Map<string, CloakTable>) (table : string) : Map<string, CloakTable> =
    let tableConfig = config.Item(table)
    let { table = target; foreignKey = foreignKey; primaryKey = primaryKey } = tableConfig.projection.Value
    let userIdKey = config.Item(target).userId.Value
    let newUserIdKey = guessUserId (data.Item table) userIdKey

    let userIds =
        data.Item target
        |> Seq.filter (fun doc -> doc.Contains(userIdKey))
        |> Seq.filter (fun doc -> doc.Contains(primaryKey))
        |> Seq.map (fun doc -> (doc.GetValue(primaryKey).ToString(), doc.GetValue(userIdKey)))
        |> Map.ofSeq

    for document in data.Item(table) do
        if document.Contains(foreignKey) then
            match Map.tryFind (document.GetValue(foreignKey).ToString()) userIds with
            | Some userId -> document.Add(newUserIdKey, userId) |> ignore
            | None -> ()

    Map.add table { tableConfig with projection = None; userId = Some(newUserIdKey) } config

let rec project (config : Map<string, CloakTable>) (data : Map<string, seq<BsonDocument>>) : unit =
    let canProject =
        config
        |> Seq.filter (fun kv -> kv.Value.projection.IsSome)
        |> Seq.filter (fun kv -> config.Item(kv.Value.projection.Value.table).userId.IsSome)
        |> Seq.map (fun kv -> kv.Key)

    match Seq.toList canProject with
    | [] -> ()
    | canProject ->
        let config = List.fold (projectOne data) config canProject
        project config data

let run (options : ParseResults<CLIArguments>) : unit =
    use stream = new StreamReader(options.GetResult Cloak_Config)
    let jsonConfig = JsonConfig.create (jsonFieldNaming = Json.snakeCase)
    let config =
        stream.ReadToEnd() |> Json.deserializeEx<CloakConfig> jsonConfig
    let conn = config |> mongoConnString |> MongoClient
    let db = conn.GetDatabase config.parameters.database

    let data = config.tables |> Map.map (fun k _ -> readCollection db k)

    async {
        let! _ =
            config.tables
            |> Seq.filter (fun kv -> Option.isSome kv.Value.decoders)
            |> Seq.map (fun kv -> decode (data.Item kv.Key) kv.Value.decoders.Value)
            |> Async.Parallel

        project config.tables data

        let! _ =
            config.tables
            |> Seq.filter (fun kv -> Option.isSome kv.Value.decoders || Option.isSome kv.Value.projection)
            |> Seq.map (fun kv -> async { writeCollection db kv.Key (data.Item kv.Key) })
            |> Async.Parallel

        ()
    } |> Async.RunSynchronously

[<EntryPoint>]
let main argv =
    try
        optionParser.ParseCommandLine(inputs = argv, raiseOnUsage = true) |> run
    with
    | :? Argu.ArguParseException as e -> printfn "%s" e.Message
    | e -> printfn "Unexpected error:\n%A" e
    0 // return an integer exit code
