module TestSimplyPythonNet.TestParser

open Xunit
open SimplyPythonNet.tokenizer
open SimplyPythonNet.parser

[<Fact>]
let ``Atom rule: None`` () =
    let result = "None" |> Tokenize |> ParseAtom
    Assert.Equal(Ok(AST.None(0u, 4u), []), result)

[<Fact>]
let ``Atom rule: True`` () =
    let result = "True" |> Tokenize |> ParseAtom
    Assert.Equal(Ok(AST.True(0u, 4u), []), result)
    
[<Fact>]
let ``Atom rule: False`` () =
    let result = "False" |> Tokenize |> ParseAtom
    Assert.Equal(Ok(AST.False(0u, 5u), []), result)
    
[<Fact>]
let ``Atom rule: ...`` () =
    let result = "..." |> Tokenize |> ParseAtom
    Assert.Equal(Ok(AST.Ellipsis(0u, 3u), []), result)
    
[<Fact>]
let ``Atom rule: 34.5e-34J`` () =
    let result = "34.5e-34J" |> Tokenize |> ParseAtom
    Assert.Equal(Ok(AST.Number(0u, 9u, "34.5e-34J"), []), result)
    
[<Fact>]
let ``Atom rule: __init__`` () =
    let result = "__init__" |> Tokenize |> ParseAtom
    Assert.Equal(Ok(AST.Name(0u, 8u, "__init__"), []), result)
    
[<Fact>]
let ``Atom rule: Single string`` () =
    let result = "r'Test'" |> Tokenize |> ParseAtom
    Assert.Equal(Ok(AST.String(0u, 7u, [ "r'Test'" ]), []), result)
    
[<Fact>]
let ``Atom rule: Double string`` () =
    let result = "r'Test' b'World'" |> Tokenize |> ParseAtom
    Assert.Equivalent(Ok(AST.String(0u, 16u, [ "r'Test'"; "b'World'" ]), []), result)
    
[<Fact>]
let ``Atom rule: empty stream`` () =
    let result = "" |> Tokenize |> ParseAtom
    Assert.Equal(Error("Unexpected end of input", 0u), result)
    
[<Fact>]
let ``Atom rule: wrong token`` () =
    let result = "for" |> Tokenize |> ParseAtom
    Assert.Equal(Error("Unexpected token", 0u), result)
    
[<Fact>]
let ``Await primary rule:`` () =
    let result = "await test" |> Tokenize |> ParseAwaitPrimary
    Assert.Equal(Ok(AST.Await(0u, 0u, AST.Name(6u, 10u, "test")), []), result)
    
[<Fact>]
let ``Await primary rule 2:`` () =
    let result = "await test +" |> Tokenize |> ParseAwaitPrimary
    Assert.Equal(Ok(AST.Await(0u, 11u, AST.Name(6u, 10u, "test")), [ Token.Plus(11u, 12u) ]), result)
    
[<Fact>]
let ``Power rule: Empty`` () =
    let result = "test" |> Tokenize |> ParsePower
    Assert.Equal(Ok(AST.Name(0u, 4u, "test"), []), result)
    
[<Fact>]
let ``Power rule: normal`` () =
    let result = "5**6" |> Tokenize |> ParsePower
    Assert.Equal(Ok(AST.Power(0u, 0u, AST.Number(0u, 1u, "5"), AST.Number(3u, 4u, "6")), []), result)
    
[<Fact>]
let ``Power rule: normal 2`` () =
    let result = "5**6 in" |> Tokenize |> ParsePower
    Assert.Equal(Ok(AST.Power(0u, 5u, AST.Number(0u, 1u, "5"), AST.Number(3u, 4u, "6")), [ Token.In(5u, 7u) ]), result)