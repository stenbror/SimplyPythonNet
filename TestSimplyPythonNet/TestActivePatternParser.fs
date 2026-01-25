module TestSimplyPythonNet.TestActivePatternParser

open Xunit
open SimplyPythonNet.ActivePatternParser


(* Tokenizer patterns unittests *)

[<Fact>]
let ``Reserved keyword: False`` () =
    match "False" |> Seq.toList, 0u  with
    |   ReservedKeywordOrLiteral(text, rest) ->
            Assert.Equal(Symbol.False(0u, 5u), text)
            Assert.Equivalent([], rest)
    |   _ -> Assert.True(false)
    
[<Fact>]
let ``Reserved keyword: None`` () =
    match "None" |> Seq.toList, 0u  with
    |   ReservedKeywordOrLiteral(text, rest) ->
            Assert.Equal(Symbol.None(0u, 4u), text)
            Assert.Equivalent([], rest)
    |   _ -> Assert.True(false)
    
[<Fact>]
let ``Reserved keyword: True`` () =
    match "True" |> Seq.toList, 0u  with
    |   ReservedKeywordOrLiteral(text, rest) ->
            Assert.Equal(Symbol.True(0u, 4u), text)
            Assert.Equivalent([], rest)
    |   _ -> Assert.True(false)
    







    
    
[<Fact>]
let ``Literal name: __init__1`` () =
    match "__init__1" |> Seq.toList, 0u  with
    |   ReservedKeywordOrLiteral(text, rest) ->
            Assert.Equal(Symbol.Name(0u, 9u, "__init__1"), text)
            Assert.Equivalent([], rest)
    |   _ -> Assert.True(false)
    
[<Fact>]
let ``Reserved keyword: Not found`` () =
    match "56.78" |> Seq.toList, 0u  with
    |   ReservedKeywordOrLiteral _ ->
            Assert.True(false)
    |   _ -> Assert.True(true)
    
    
(* Expression parser unittests *)
    
[<Fact>]
let ``Primary Expression rule: await expression`` () =
    let result = [ Symbol.Await(0u, 4u); Symbol.Name(6u, 14u, "__init__") ] |> Parse
    Assert.Equivalent((AST.Await(0u, 0u, AST.Name(6u, 14u, "__init__")), []), result)
    
[<Fact>]
let ``Sum rule: plus and minus`` () =
    let result = [ Symbol.Name(0u, 1u, "a"); Symbol.Plus(1u, 2u); Symbol.Name(2u, 3u, "b"); Symbol.Minus(3u, 4u); Symbol.Number(4u,5u,"1"); Symbol.SemiColon(5u, 6u)] |> Parse
    Assert.Equivalent((AST.Minus(0u, 5u, AST.Plus(0u, 3u, AST.Name(0u, 1u, "a"), AST.Name(2u, 3u, "b")), AST.Number(4u, 5u, "1")), [ Symbol.SemiColon(5u, 6u) ]), result)
    
[<Fact>]
let ``Atom rule: strings`` () =
    let result = [ Symbol.String(0u, 22u, "'Hello, World!'"); Symbol.String(0u, 1u, "r'Test'"); Symbol.SemiColon(22u, 23u) ] |> Parse
    Assert.Equivalent((AST.String(0u, 22u, ["'Hello, World!'"; "r'Test'"]), [ Symbol.SemiColon(22u, 23u) ]), result)