open Argu
open FSharp.Json
open MongoCleanup
open MongoDB.Bson
open MongoDB.Driver
open System
open System.Collections.Generic
open System.IO
open System.Security.Cryptography

type CLIArguments =
    | [<Mandatory>] Cloak_Config of path : string
    interface IArgParserTemplate with
        member s.Usage =
            match s with
            | Cloak_Config _ -> "specify the cloak config describing the data source"

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

type DeleteIf =
    { field : string
      values : string list }

type CloakTable =
    { userId : string option
      decoders : Decoder list option
      projection : Projection option
      query : string option
      deleteIf : DeleteIf list option }

type CloakConfig =
    { parameters : ConnectionProperties
      tables : Map<string, CloakTable> }

type BsonUpdate = BsonValue -> BsonValue

type UserIds = Map<Projection * string, BsonValue>

let optionParser = ArgumentParser.Create<CLIArguments>(programName = "MongoCleanup")

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
            match document.GetValue(key).BsonType with
            | BsonType.Document -> update' (document.GetValue(key).AsBsonDocument) rest f
            | BsonType.Array ->
                for item in document.GetValue(key).AsBsonArray do
                    update' item.AsBsonDocument rest f
            | _ -> ()

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
    safely (fun () ->
        value.AsString
        |> System.Int64.Parse
        |> BsonInt64)

let textToReal (value : BsonValue) : BsonValue =
    safely (fun () ->
        value.AsString
        |> System.Double.Parse
        |> BsonDouble)

let textToDate (value : BsonValue) : BsonValue =
    safely (fun () -> System.DateTime.Parse(value.AsString) |> BsonDateTime)
let realToInteger (value : BsonValue) : BsonValue = safely (fun () -> BsonInt64(int64 value.AsDouble))
let realToBoolean (value : BsonValue) : BsonValue = safely (fun () -> BsonBoolean(value.AsDouble <> 0.0))

let textToBoolean (value : BsonValue) : BsonValue =
    safely (fun () ->
        value.AsString
        |> System.Boolean.Parse
        |> BsonBoolean)

let base64 (value : BsonValue) : BsonValue =
    safely (fun () ->
        value.AsString
        |> System.Convert.FromBase64String
        |> BsonBinaryData)

let substring (from : int32) (length : int32) (value : BsonValue) : BsonValue =
    safely (fun () -> value.AsString.Substring(from, length) |> BsonString)

let convertToBytes (value : BsonValue) : byte [] =
    if value.IsBsonBinaryData then value.AsByteArray
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
        with _ -> bsonNull

let substringDecoder (decoder : Decoder) =
    match (decoder.from, decoder.length) with
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
    with e -> printfn "Error %A when applying %A" e.Message decoder

let applyDecoders (decoders : Decoder list) (document : BsonDocument) : unit =
    for decoder in decoders do
        applyDecoder document decoder

let readCollection (db : IMongoDatabase) (name : string) : seq<BsonDocument> =
    seq {
        for doc in (db.GetCollection<BsonDocument>(name).Find(fun _ -> true).ToCursor().ToEnumerable()) do
            yield doc
    }

let shouldDelete (config : CloakTable) (doc : BsonDocument) : bool =
    match config.deleteIf with
    | None -> false
    | Some(deleteIf) ->
        deleteIf
        |> Seq.exists
               (fun deleteIf ->
               doc.Contains(deleteIf.field)
               && Seq.exists ((=) (doc.GetValue(deleteIf.field).ToString())) deleteIf.values)

let docToWriteModel (config : CloakTable) (doc : BsonDocument) : WriteModel<BsonDocument> =
    let field = StringFieldDefinition<BsonDocument, BsonValue>("_id")
    let filter = Builders<BsonDocument>.Filter.Eq(field, doc.GetValue("_id"))
    if shouldDelete config doc then upcast DeleteOneModel<BsonDocument>(filter)
    else upcast ReplaceOneModel<BsonDocument>(filter, doc)

let writeCollection (db : IMongoDatabase) (name : string) (config : CloakTable) (documents : seq<BsonDocument>) : unit =
    let collection = db.GetCollection<BsonDocument>(name)

    let updates =
        documents
        |> Seq.map (docToWriteModel config)
        |> Seq.cast
    collection.BulkWrite(updates) |> ignore

let decode (documents : seq<BsonDocument>) (decoders : Decoder list option) : unit =
    match decoders with
    | None -> ()
    | Some decoders ->
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
    sprintf "mongodb://%s%s:%i/%s" userpass options.hostname port options.database

let acUserId = "_ac_user_id"

let guessUserId (data : seq<BsonDocument>) (proposed : string) : string =
    if Seq.forall (fun (x : BsonDocument) -> not (x.Contains proposed)) data then proposed
    else acUserId

let rec projectionChain (config : Map<string, CloakTable>) (table : CloakTable) : string list =
    match table.projection with
    | None -> []
    | Some(projection) -> projection.table :: projectionChain config (config.Item projection.table)

let dependsOn (config : Map<string, CloakTable>) (table1 : KeyValuePair<string, CloakTable>)
    (table2 : KeyValuePair<string, CloakTable>) : int =
    if Seq.contains table2.Key (projectionChain config table1.Value) then 1
    elif Seq.contains table1.Key (projectionChain config table2.Value) then -1
    else 0

let rec userId (config : CloakConfig) (table : string) : string =
    match config.tables.Item(table).projection with
    | Some projection -> userId config projection.table
    | None -> Option.defaultValue "_ac_user_id" (config.tables.Item(table).userId)

let projectWithUserIds (batch : seq<BsonDocument>) (table : string) (config : CloakConfig) (userIds : UserIds) : unit =
    let userIdField = userId config table
    match config.tables.Item(table).projection with
    | None -> ()
    | Some projection ->
        for document in batch do
            if document.Contains(projection.foreignKey) then
                match Map.tryFind (projection, document.GetValue(projection.foreignKey).ToString()) userIds with
                | Some userId ->
                    document.Remove(userIdField)
                    document.Add(userIdField, userId) |> ignore
                | None -> ()

let saveUserIds (userIds : UserIds) (batch : seq<BsonDocument>) (table : string) (config : CloakConfig) : UserIds =
    let userId = userId config table

    let toPreserve =
        config.tables
        |> Seq.choose (fun table -> table.Value.projection)
        |> Seq.filter (fun projection -> projection.table = table)

    let saveUserIds'' =
        fun (document : BsonDocument) userIds projection ->
            if document.Contains(projection.primaryKey) && document.Contains(userId) then
                Map.add (projection, document.GetValue(projection.primaryKey).ToString()) (document.GetValue(userId))
                    userIds
            else userIds

    let saveUserIds' = fun userIds document -> Seq.fold (saveUserIds'' document) userIds toPreserve
    Seq.fold saveUserIds' userIds batch

let processOne (db : IMongoDatabase) (config : CloakConfig) (table : string) (userIds : UserIds)
    (batch : seq<BsonDocument>) : UserIds =
    let tableConfig = config.tables.Item(table)
    decode batch tableConfig.decoders
    projectWithUserIds batch table config userIds
    writeCollection db table tableConfig batch
    saveUserIds userIds batch table config

let run (options : ParseResults<CLIArguments>) : unit =
    use stream = new StreamReader(options.GetResult Cloak_Config)
    let jsonConfig = JsonConfig.create (jsonFieldNaming = Json.snakeCase)
    let config = stream.ReadToEnd() |> Json.deserializeEx<CloakConfig> jsonConfig
    printfn "Connecting..."
    let conn =
        config
        |> mongoConnString
        |> MongoClient

    let db = conn.GetDatabase config.parameters.database
    printfn "Reading data..."
    let toProcess =
        config.tables
        |> Seq.filter (fun kv -> Option.isSome kv.Value.decoders || Option.isSome kv.Value.projection)
        |> Seq.sortWith (dependsOn config.tables)

    let mutable userIds = Map.empty
    for kv in toProcess do
        printfn "Processing %s" kv.Key
        userIds <- readCollection db kv.Key
                   |> Seq.chunkBySize 100
                   |> Seq.fold (processOne db config kv.Key) userIds

[<EntryPoint>]
let main argv =
    try
        optionParser.ParseCommandLine(inputs = argv, raiseOnUsage = true) |> run
        printfn "Success"
    with
    | :? Argu.ArguParseException as e -> printfn "%s" e.Message
    | e ->
        printfn "Unexpected error:\n%A" e
    0 // return an integer exit code
