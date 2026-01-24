module TestSimplyPythonNet.TestActivePatternParser

open Xunit
open SimplyPythonNet.ActivePatternParser

[<Fact>]
let ``Primary Expression rule: await expression`` () =
    let result = [ Symbol.Await(0u, 4u); Symbol.Name(6u, 14u, "__init__") ] |> Parse
    Assert.Equivalent((AST.Await(0u, 0u, AST.Name(6u, 14u, "__init__")), []), result)