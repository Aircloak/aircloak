namespace MongoCleanup.Test

open Microsoft.VisualStudio.TestTools.UnitTesting
open MongoDB.Bson
open Program

[<TestClass>]
type TestTextToInteger() =

    [<TestMethod>]
    member this.TestWithValidInput() =
        let result = BsonString("123").AsBsonValue |> textToInteger
        Assert.AreEqual(result.AsInt64, 123L)

    [<TestMethod>]
    member this.TestWithInvalidInput() =
        Assert.AreEqual(BsonDouble(2.23).AsBsonValue |> textToInteger, BsonNull.Value)

    [<TestMethod>]
    member this.TestWithInvalidFormat() =
        Assert.AreEqual(BsonString("bob") |> textToInteger, BsonNull.Value)

[<TestClass>]
type TestTextToReal() =

    [<TestMethod>]
    member this.TestWithValidInput() =
        let result = BsonString("123.23").AsBsonValue |> textToReal
        Assert.AreEqual(result.AsDouble, 123.23)

    [<TestMethod>]
    member this.TestWithInvalidInput() =
        Assert.AreEqual(BsonDouble(2.23).AsBsonValue |> textToReal, BsonNull.Value)

    [<TestMethod>]
    member this.TestWithInvalidFormat() =
        Assert.AreEqual(BsonString("bob") |> textToReal, BsonNull.Value)
