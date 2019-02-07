namespace MongoCleanup.Test

open Microsoft.VisualStudio.TestTools.UnitTesting
open MongoDB.Bson
open Program

[<TestClass>]
type TestDecoders() =

    [<TestMethod>]
    member this.TestWithInvalidInput() =
        for decoder in [ textToInteger; textToReal; textToDate; realToInteger; textToBoolean ] do
            Assert.AreEqual(BsonNull.Value, BsonNull.Value |> decoder)

    [<TestMethod>]
    member this.TestWithInvalidFormat() =
        for decoder in [ textToInteger; textToReal; textToDate; realToInteger; textToBoolean ] do
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

    [<TestMethod>]
    member this.TestRealToInteger() =
        let result = BsonDouble(2.23).AsBsonValue |> realToInteger
        Assert.AreEqual(2L, result.AsInt64)

    [<TestMethod>]
    member this.TestTextToBoolean() =
        Assert.AreEqual(BsonBoolean.True, BsonString("TRUE").AsBsonValue |> textToBoolean)
        Assert.AreEqual(BsonBoolean.False, BsonString("false").AsBsonValue |> textToBoolean)

