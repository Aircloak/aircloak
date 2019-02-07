namespace MongoCleanup.Test

open Microsoft.VisualStudio.TestTools.UnitTesting
open MongoDB.Bson
open Program

[<TestClass>]
type TestDecoders() =

    [<TestMethod>]
    member this.TestWithInvalidInput() =
        for decoder in [ textToInteger; textToReal; textToDate ] do
            Assert.AreEqual(BsonNull.Value, BsonDouble(2.23).AsBsonValue |> decoder)

    [<TestMethod>]
    member this.TestWithInvalidFormat() =
        for decoder in [ textToInteger; textToReal; textToDate ] do
            Assert.AreEqual(BsonNull.Value, BsonString("bob") |> decoder)

    [<TestMethod>]
    member this.TestTextToInteger() =
        let result = BsonString("123").AsBsonValue |> textToInteger
        Assert.AreEqual(123L, result.AsInt64)

    [<TestMethod>]
    member this.TestTextToReal() =
        let result = BsonString("123.23").AsBsonValue |> textToReal
        Assert.AreEqual(123.23, result.AsDouble)

    [<TestMethod>]
    member this.TestTextToDate() =
        let result = BsonString("2018-01-01").AsBsonValue |> textToDate
        Assert.AreEqual(System.DateTime.Parse("2018-01-01T00:00:00").ToUniversalTime(), result.ToUniversalTime())

