namespace FSharpTest
open System
open NUnit.Framework
open FsharpLibs

[<TestFixture>]
type Test() = 

    [<Test>]
    member x.TestCase() =
        let allArticlesArray = Playground.ConfigureArticles() 

        let allArticlesLength = Array.length allArticlesArray

        let allArticlesEquals = allArticlesLength = 53

        Assert.IsTrue( allArticlesEquals )

