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