module TestSimplyPythonNet.TestActivePatternParser

open Xunit
open SimplyPythonNet.ActivePatternParser

[<Fact>]
let ``Primary Expression rule: await expression`` () =
    let result = [ Symbol.Await(0u, 4u); Symbol.Name(6u, 14u, "__init__") ] |> Parse
    Assert.Equivalent((AST.Await(0u, 0u, AST.Name(6u, 14u, "__init__")), []), result)
    
[<Fact>]
let ``Sum rule: plus and minus`` () =
    let result = [ Symbol.Name(0u, 1u, "a"); Symbol.Plus(1u, 2u); Symbol.Name(2u, 3u, "b"); Symbol.Minus(3u, 4u); Symbol.Number(4u,5u,"1"); Symbol.SemiColon(5u, 6u)] |> Parse
    Assert.Equivalent((AST.Minus(0u, 5u, AST.Plus(0u, 3u, AST.Name(0u, 1u, "a"), AST.Name(2u, 3u, "b")), AST.Number(4u, 5u, "1")), [ Symbol.SemiColon(5u, 6u) ]), result)