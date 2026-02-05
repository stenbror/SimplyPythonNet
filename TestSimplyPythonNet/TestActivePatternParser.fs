

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
let ``Reserved keyword: and`` () =
    match "and" |> Seq.toList, 0u  with
    |   ReservedKeywordOrLiteral(text, rest) ->
            Assert.Equal(Symbol.And(0u, 3u), text)
            Assert.Equivalent([], rest)
    |   _ -> Assert.True(false)
    
[<Fact>]
let ``Reserved keyword: as`` () =
    match "as" |> Seq.toList, 0u  with
    |   ReservedKeywordOrLiteral(text, rest) ->
            Assert.Equal(Symbol.As(0u, 2u), text)
            Assert.Equivalent([], rest)
    |   _ -> Assert.True(false)
    
[<Fact>]
let ``Reserved keyword: assert`` () =
    match "assert" |> Seq.toList, 0u  with
    |   ReservedKeywordOrLiteral(text, rest) ->
            Assert.Equal(Symbol.Assert(0u, 6u), text)
            Assert.Equivalent([], rest)
    |   _ -> Assert.True(false)

[<Fact>]
let ``Reserved keyword: async`` () =
    match "async" |> Seq.toList, 0u  with
    |   ReservedKeywordOrLiteral(text, rest) ->
            Assert.Equal(Symbol.Async(0u, 5u), text)
            Assert.Equivalent([], rest)
    |   _ -> Assert.True(false)
    
[<Fact>]
let ``Reserved keyword: await`` () =
    match "await" |> Seq.toList, 0u  with
    |   ReservedKeywordOrLiteral(text, rest) ->
            Assert.Equal(Symbol.Await(0u, 5u), text)
            Assert.Equivalent([], rest)
    |   _ -> Assert.True(false)
    
[<Fact>]
let ``Reserved keyword: break`` () =
    match "break" |> Seq.toList, 0u  with
    |   ReservedKeywordOrLiteral(text, rest) ->
            Assert.Equal(Symbol.Break(0u, 5u), text)
            Assert.Equivalent([], rest)
    |   _ -> Assert.True(false)
    
[<Fact>]
let ``Reserved keyword: class`` () =
    match "class" |> Seq.toList, 0u  with
    |   ReservedKeywordOrLiteral(text, rest) ->
            Assert.Equal(Symbol.Class(0u, 5u), text)
            Assert.Equivalent([], rest)
    |   _ -> Assert.True(false)
    
[<Fact>]
let ``Reserved keyword: continue`` () =
    match "continue" |> Seq.toList, 0u  with
    |   ReservedKeywordOrLiteral(text, rest) ->
            Assert.Equal(Symbol.Continue(0u, 8u), text)
            Assert.Equivalent([], rest)
    |   _ -> Assert.True(false)
    
[<Fact>]
let ``Reserved keyword: del`` () =
    match "del" |> Seq.toList, 0u  with
    |   ReservedKeywordOrLiteral(text, rest) ->
            Assert.Equal(Symbol.Del(0u, 3u), text)
            Assert.Equivalent([], rest)
    |   _ -> Assert.True(false)
    
[<Fact>]
let ``Reserved keyword: def`` () =
    match "def" |> Seq.toList, 0u  with
    |   ReservedKeywordOrLiteral(text, rest) ->
            Assert.Equal(Symbol.Def(0u, 3u), text)
            Assert.Equivalent([], rest)
    |   _ -> Assert.True(false)
    
[<Fact>]
let ``Reserved keyword: elif`` () =
    match "elif" |> Seq.toList, 0u  with
    |   ReservedKeywordOrLiteral(text, rest) ->
            Assert.Equal(Symbol.Elif(0u, 4u), text)
            Assert.Equivalent([], rest)
    |   _ -> Assert.True(false)
    
[<Fact>]
let ``Reserved keyword: else`` () =
    match "else" |> Seq.toList, 0u  with
    |   ReservedKeywordOrLiteral(text, rest) ->
            Assert.Equal(Symbol.Else(0u, 4u), text)
            Assert.Equivalent([], rest)
    |   _ -> Assert.True(false)

[<Fact>]
let ``Reserved keyword: except`` () =
    match "except" |> Seq.toList, 0u  with
    |   ReservedKeywordOrLiteral(text, rest) ->
            Assert.Equal(Symbol.Except(0u, 6u), text)
            Assert.Equivalent([], rest)
    |   _ -> Assert.True(false)

[<Fact>]
let ``Reserved keyword: finally`` () =
    match "finally" |> Seq.toList, 0u  with
    |   ReservedKeywordOrLiteral(text, rest) ->
            Assert.Equal(Symbol.Finally(0u, 7u), text)
            Assert.Equivalent([], rest)
    |   _ -> Assert.True(false)

[<Fact>]
let ``Reserved keyword: for`` () =
    match "for" |> Seq.toList, 0u  with
    |   ReservedKeywordOrLiteral(text, rest) ->
            Assert.Equal(Symbol.For(0u, 3u), text)
            Assert.Equivalent([], rest)
    |   _ -> Assert.True(false)
    
[<Fact>]
let ``Reserved keyword: from`` () =
    match "from" |> Seq.toList, 0u  with
    |   ReservedKeywordOrLiteral(text, rest) ->
            Assert.Equal(Symbol.From(0u, 4u), text)
            Assert.Equivalent([], rest)
    |   _ -> Assert.True(false)
    
[<Fact>]
let ``Reserved keyword: global`` () =
    match "global" |> Seq.toList, 0u  with
    |   ReservedKeywordOrLiteral(text, rest) ->
            Assert.Equal(Symbol.Global(0u, 6u), text)
            Assert.Equivalent([], rest)
    |   _ -> Assert.True(false)
    
[<Fact>]
let ``Reserved keyword: if`` () =
    match "if" |> Seq.toList, 0u  with
    |   ReservedKeywordOrLiteral(text, rest) ->
            Assert.Equal(Symbol.If(0u, 2u), text)
            Assert.Equivalent([], rest)
    |   _ -> Assert.True(false)
    
[<Fact>]
let ``Reserved keyword: import`` () =
    match "import" |> Seq.toList, 0u  with
    |   ReservedKeywordOrLiteral(text, rest) ->
            Assert.Equal(Symbol.Import(0u, 6u), text)
            Assert.Equivalent([], rest)
    |   _ -> Assert.True(false)
    
[<Fact>]
let ``Reserved keyword: in`` () =
    match "in" |> Seq.toList, 0u  with
    |   ReservedKeywordOrLiteral(text, rest) ->
            Assert.Equal(Symbol.In(0u, 2u), text)
            Assert.Equivalent([], rest)
    |   _ -> Assert.True(false)
    
[<Fact>]
let ``Reserved keyword: is`` () =
    match "is" |> Seq.toList, 0u  with
    |   ReservedKeywordOrLiteral(text, rest) ->
            Assert.Equal(Symbol.Is(0u, 2u), text)
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
let ``Reserved keyword: lambda`` () =
    match "lambda" |> Seq.toList, 0u  with
    |   ReservedKeywordOrLiteral(text, rest) ->
            Assert.Equal(Symbol.Lambda(0u, 6u), text)
            Assert.Equivalent([], rest)
    |   _ -> Assert.True(false)
    
[<Fact>]
let ``Reserved keyword: nonlocal`` () =
    match "nonlocal" |> Seq.toList, 0u  with
    |   ReservedKeywordOrLiteral(text, rest) ->
            Assert.Equal(Symbol.Nonlocal(0u, 7u), text)
            Assert.Equivalent([], rest)
    |   _ -> Assert.True(false)
    
[<Fact>]
let ``Reserved keyword: not`` () =
    match "not" |> Seq.toList, 0u  with
    |   ReservedKeywordOrLiteral(text, rest) ->
            Assert.Equal(Symbol.Not(0u, 3u), text)
            Assert.Equivalent([], rest)
    |   _ -> Assert.True(false)
    
[<Fact>]
let ``Reserved keyword: or`` () =
    match "or" |> Seq.toList, 0u  with
    |   ReservedKeywordOrLiteral(text, rest) ->
            Assert.Equal(Symbol.Or(0u, 2u), text)
            Assert.Equivalent([], rest)
    |   _ -> Assert.True(false)
    
[<Fact>]
let ``Reserved keyword: pass`` () =
    match "pass" |> Seq.toList, 0u  with
    |   ReservedKeywordOrLiteral(text, rest) ->
            Assert.Equal(Symbol.Pass(0u, 4u), text)
            Assert.Equivalent([], rest)
    |   _ -> Assert.True(false)
    
[<Fact>]
let ``Reserved keyword: raise`` () =
    match "raise" |> Seq.toList, 0u  with
    |   ReservedKeywordOrLiteral(text, rest) ->
            Assert.Equal(Symbol.Raise(0u, 5u), text)
            Assert.Equivalent([], rest)
    |   _ -> Assert.True(false)
    
[<Fact>]
let ``Reserved keyword: return`` () =
    match "return" |> Seq.toList, 0u  with
    |   ReservedKeywordOrLiteral(text, rest) ->
            Assert.Equal(Symbol.Return(0u, 6u), text)
            Assert.Equivalent([], rest)
    |   _ -> Assert.True(false)
    
[<Fact>]
let ``Reserved keyword: try`` () =
    match "try" |> Seq.toList, 0u  with
    |   ReservedKeywordOrLiteral(text, rest) ->
            Assert.Equal(Symbol.Try(0u, 3u), text)
            Assert.Equivalent([], rest)
    |   _ -> Assert.True(false)
    
[<Fact>]
let ``Reserved keyword: while`` () =
    match "while" |> Seq.toList, 0u  with
    |   ReservedKeywordOrLiteral(text, rest) ->
            Assert.Equal(Symbol.While(0u, 5u), text)
            Assert.Equivalent([], rest)
    |   _ -> Assert.True(false)
    
[<Fact>]
let ``Reserved keyword: with`` () =
    match "with" |> Seq.toList, 0u  with
    |   ReservedKeywordOrLiteral(text, rest) ->
            Assert.Equal(Symbol.With(0u, 4u), text)
            Assert.Equivalent([], rest)
    |   _ -> Assert.True(false)
    
[<Fact>]
let ``Reserved keyword: yield`` () =
    match "yield" |> Seq.toList, 0u  with
    |   ReservedKeywordOrLiteral(text, rest) ->
            Assert.Equal(Symbol.Yield(0u, 5u), text)
            Assert.Equivalent([], rest)
    |   _ -> Assert.True(false)
    
[<Fact>]
let ``Reserved keyword: Not found`` () =
    match "56.78" |> Seq.toList, 0u  with
    |   ReservedKeywordOrLiteral _ ->
            Assert.True(false)
    |   _ -> Assert.True(true)
    
(* Operator patterns unittests *)

[<Fact>]
let ``Operator: <<=`` () =
    match "<<=" |> Seq.toList, 0u  with
    |   OperatorOrDelimiter(text, rest) ->
            Assert.Equal(Symbol.ShiftLeftAssign(0u,3u), text)
            Assert.Equivalent([], rest)
    |   _ -> Assert.True(false)
    
[<Fact>]
let ``Operator: >>=`` () =
    match ">>=" |> Seq.toList, 0u  with
    |   OperatorOrDelimiter(text, rest) ->
            Assert.Equal(Symbol.ShiftRightAssign(0u,3u), text)
            Assert.Equivalent([], rest)
    |   _ -> Assert.True(false)
    
[<Fact>]
let ``Operator: **=`` () =
    match "**=" |> Seq.toList, 0u  with
    |   OperatorOrDelimiter(text, rest) ->
            Assert.Equal(Symbol.PowerAssign(0u,3u), text)
            Assert.Equivalent([], rest)
    |   _ -> Assert.True(false)
    
[<Fact>]
let ``Operator: //=`` () =
    match "//=" |> Seq.toList, 0u  with
    |   OperatorOrDelimiter(text, rest) ->
            Assert.Equal(Symbol.FloorDivideAssign(0u,3u), text)
            Assert.Equivalent([], rest)
    |   _ -> Assert.True(false)
    
[<Fact>]
let ``Operator: ...`` () =
    match "..." |> Seq.toList, 0u  with
    |   OperatorOrDelimiter(text, rest) ->
            Assert.Equal(Symbol.Ellipsis(0u,3u), text)
            Assert.Equivalent([], rest)
    |   _ -> Assert.True(false)
    
[<Fact>]
let ``Operator: +=`` () =
    match "+=" |> Seq.toList, 0u  with
    |   OperatorOrDelimiter(text, rest) ->
            Assert.Equal(Symbol.PlusEqual(0u,2u), text)
            Assert.Equivalent([], rest)
    |   _ -> Assert.True(false)
    
[<Fact>]
let ``Operator: -=`` () =
    match "-=" |> Seq.toList, 0u  with
    |   OperatorOrDelimiter(text, rest) ->
            Assert.Equal(Symbol.MinusEqual(0u,2u), text)
            Assert.Equivalent([], rest)
    |   _ -> Assert.True(false)
    
[<Fact>]
let ``Operator: *=`` () =
    match "*=" |> Seq.toList, 0u  with
    |   OperatorOrDelimiter(text, rest) ->
            Assert.Equal(Symbol.MultiplyEqual(0u,2u), text)
            Assert.Equivalent([], rest)
    |   _ -> Assert.True(false)
    
[<Fact>]
let ``Operator: /=`` () =
    match "/=" |> Seq.toList, 0u  with
    |   OperatorOrDelimiter(text, rest) ->
            Assert.Equal(Symbol.DivideEqual(0u,2u), text)
            Assert.Equivalent([], rest)
    |   _ -> Assert.True(false)
    
[<Fact>]
let ``Operator: %=`` () =
    match "%=" |> Seq.toList, 0u  with
    |   OperatorOrDelimiter(text, rest) ->
            Assert.Equal(Symbol.ModuloEqual(0u,2u), text)
            Assert.Equivalent([], rest)
    |   _ -> Assert.True(false)
    
[<Fact>]
let ``Operator: &=`` () =
    match "&=" |> Seq.toList, 0u  with
    |   OperatorOrDelimiter(text, rest) ->
            Assert.Equal(Symbol.BitwiseAndEqual(0u,2u), text)
            Assert.Equivalent([], rest)
    |   _ -> Assert.True(false)
    
[<Fact>]
let ``Operator: |=`` () =
    match "|=" |> Seq.toList, 0u  with
    |   OperatorOrDelimiter(text, rest) ->
            Assert.Equal(Symbol.BitwiseOrEqual(0u,2u), text)
            Assert.Equivalent([], rest)
    |   _ -> Assert.True(false)
    
[<Fact>]
let ``Operator: ^=`` () =
    match "^=" |> Seq.toList, 0u  with
    |   OperatorOrDelimiter(text, rest) ->
            Assert.Equal(Symbol.BitwiseXorEqual(0u,2u), text)
            Assert.Equivalent([], rest)
    |   _ -> Assert.True(false)
    
[<Fact>]
let ``Operator: Matrice=`` () =
    match "@=" |> Seq.toList, 0u  with
    |   OperatorOrDelimiter(text, rest) ->
            Assert.Equal(Symbol.MatricesEqual(0u,2u), text)
            Assert.Equivalent([], rest)
    |   _ -> Assert.True(false)
    
[<Fact>]
let ``Operator: :=`` () =
    match ":=" |> Seq.toList, 0u  with
    |   OperatorOrDelimiter(text, rest) ->
            Assert.Equal(Symbol.ColonEqual(0u,2u), text)
            Assert.Equivalent([], rest)
    |   _ -> Assert.True(false)
    
[<Fact>]
let ``Operator: ->`` () =
    match "->" |> Seq.toList, 0u  with
    |   OperatorOrDelimiter(text, rest) ->
            Assert.Equal(Symbol.Arrow(0u,2u), text)
            Assert.Equivalent([], rest)
    |   _ -> Assert.True(false)
    
[<Fact>]
let ``Operator:==`` () =
    match "==" |> Seq.toList, 0u  with
    |   OperatorOrDelimiter(text, rest) ->
            Assert.Equal(Symbol.Equal(0u,2u), text)
            Assert.Equivalent([], rest)
    |   _ -> Assert.True(false)
    
[<Fact>]
let ``Operator: **`` () =
    match "**" |> Seq.toList, 0u  with
    |   OperatorOrDelimiter(text, rest) ->
            Assert.Equal(Symbol.Power(0u,2u), text)
            Assert.Equivalent([], rest)
    |   _ -> Assert.True(false)
    
[<Fact>]
let ``Operator: //`` () =
    match "//" |> Seq.toList, 0u  with
    |   OperatorOrDelimiter(text, rest) ->
            Assert.Equal(Symbol.FloorDivide(0u,2u), text)
            Assert.Equivalent([], rest)
    |   _ -> Assert.True(false)
    
[<Fact>]
let ``Operator: &`` () =
    match "&" |> Seq.toList, 0u  with
    |   OperatorOrDelimiter(text, rest) ->
            Assert.Equal(Symbol.BitwiseAnd(0u,1u), text)
            Assert.Equivalent([], rest)
    |   _ -> Assert.True(false)
    
[<Fact>]
let ``Operator: |`` () =
    match "|" |> Seq.toList, 0u  with
    |   OperatorOrDelimiter(text, rest) ->
            Assert.Equal(Symbol.BitwiseOr(0u,1u), text)
            Assert.Equivalent([], rest)
    |   _ -> Assert.True(false)
    
[<Fact>]
let ``Operator: ^`` () =
    match "^" |> Seq.toList, 0u  with
    |   OperatorOrDelimiter(text, rest) ->
            Assert.Equal(Symbol.BitwiseXor(0u,1u), text)
            Assert.Equivalent([], rest)
    |   _ -> Assert.True(false)
    
[<Fact>]
let ``Operator: ~`` () =
    match "~" |> Seq.toList, 0u  with
    |   OperatorOrDelimiter(text, rest) ->
            Assert.Equal(Symbol.BitwiseInvert(0u,1u), text)
            Assert.Equivalent([], rest)
    |   _ -> Assert.True(false)
    
[<Fact>]
let ``Operator: <`` () =
    match "<" |> Seq.toList, 0u  with
    |   OperatorOrDelimiter(text, rest) ->
            Assert.Equal(Symbol.Less(0u,1u), text)
            Assert.Equivalent([], rest)
    |   _ -> Assert.True(false)
    
[<Fact>]
let ``Operator: >`` () =
    match ">" |> Seq.toList, 0u  with
    |   OperatorOrDelimiter(text, rest) ->
            Assert.Equal(Symbol.Greater(0u,1u), text)
            Assert.Equivalent([], rest)
    |   _ -> Assert.True(false)
    
[<Fact>]
let ``Operator: (`` () =
    match "(" |> Seq.toList, 0u  with
    |   OperatorOrDelimiter(text, rest) ->
            Assert.Equal(Symbol.LeftParenthesis(0u,1u), text)
            Assert.Equivalent([], rest)
    |   _ -> Assert.True(false)
    
[<Fact>]
let ``Operator: )`` () =
    match ")" |> Seq.toList, 0u  with
    |   OperatorOrDelimiter(text, rest) ->
            Assert.Equal(Symbol.RightParenthesis(0u,1u), text)
            Assert.Equivalent([], rest)
    |   _ -> Assert.True(false)
    
[<Fact>]
let ``Operator: [`` () =
    match "[" |> Seq.toList, 0u  with
    |   OperatorOrDelimiter(text, rest) ->
            Assert.Equal(Symbol.LeftSquareBracket(0u,1u), text)
            Assert.Equivalent([], rest)
    |   _ -> Assert.True(false)
    
[<Fact>]
let ``Operator: ]`` () =
    match "]" |> Seq.toList, 0u  with
    |   OperatorOrDelimiter(text, rest) ->
            Assert.Equal(Symbol.RightSquareBracket(0u,1u), text)
            Assert.Equivalent([], rest)
    |   _ -> Assert.True(false)
    
[<Fact>]
let ``Operator: {`` () =
    match "{" |> Seq.toList, 0u  with
    |   OperatorOrDelimiter(text, rest) ->
            Assert.Equal(Symbol.LeftCurlyBracket(0u,1u), text)
            Assert.Equivalent([], rest)
    |   _ -> Assert.True(false)
    
[<Fact>]
let ``Operator: }`` () =
    match "}" |> Seq.toList, 0u  with
    |   OperatorOrDelimiter(text, rest) ->
            Assert.Equal(Symbol.RightCurlyBracket(0u,1u), text)
            Assert.Equivalent([], rest)
    |   _ -> Assert.True(false)
    
[<Fact>]
let ``Operator: ,`` () =
    match "," |> Seq.toList, 0u  with
    |   OperatorOrDelimiter(text, rest) ->
            Assert.Equal(Symbol.Comma(0u,1u), text)
            Assert.Equivalent([], rest)
    |   _ -> Assert.True(false)
    
[<Fact>]
let ``Operator: ;`` () =
    match ";" |> Seq.toList, 0u  with
    |   OperatorOrDelimiter(text, rest) ->
            Assert.Equal(Symbol.SemiColon(0u,1u), text)
            Assert.Equivalent([], rest)
    |   _ -> Assert.True(false)
    
[<Fact>]
let ``Operator: !`` () =
    match "!" |> Seq.toList, 0u  with
    |   OperatorOrDelimiter(text, rest) ->
            Assert.Equal(Symbol.Not(0u,1u), text)
            Assert.Equivalent([], rest)
    |   _ -> Assert.True(false)
    
[<Fact>]
let ``Operator: :`` () =
    match ":" |> Seq.toList, 0u  with
    |   OperatorOrDelimiter(text, rest) ->
            Assert.Equal(Symbol.Colon(0u,1u), text)
            Assert.Equivalent([], rest)
    |   _ -> Assert.True(false)
    
[<Fact>]
let ``Operator: =`` () =
    match "=" |> Seq.toList, 0u  with
    |   OperatorOrDelimiter(text, rest) ->
            Assert.Equal(Symbol.Assign(0u,1u), text)
            Assert.Equivalent([], rest)
    |   _ -> Assert.True(false)
    
[<Fact>]
let ``Operator: +`` () =
    match "+" |> Seq.toList, 0u  with
    |   OperatorOrDelimiter(text, rest) ->
            Assert.Equal(Symbol.Plus(0u,1u), text)
            Assert.Equivalent([], rest)
    |   _ -> Assert.True(false)
    
[<Fact>]
let ``Operator: -`` () =
    match "-" |> Seq.toList, 0u  with
    |   OperatorOrDelimiter(text, rest) ->
            Assert.Equal(Symbol.Minus(0u,1u), text)
            Assert.Equivalent([], rest)
    |   _ -> Assert.True(false)
    
[<Fact>]
let ``Operator: *`` () =
    match "*" |> Seq.toList, 0u  with
    |   OperatorOrDelimiter(text, rest) ->
            Assert.Equal(Symbol.Multiply(0u,1u), text)
            Assert.Equivalent([], rest)
    |   _ -> Assert.True(false)
    
[<Fact>]
let ``Operator: /`` () =
    match "/" |> Seq.toList, 0u  with
    |   OperatorOrDelimiter(text, rest) ->
            Assert.Equal(Symbol.Divide(0u,1u), text)
            Assert.Equivalent([], rest)
    |   _ -> Assert.True(false)
    
[<Fact>]
let ``Operator: %`` () =
    match "%" |> Seq.toList, 0u  with
    |   OperatorOrDelimiter(text, rest) ->
            Assert.Equal(Symbol.Modulo(0u,1u), text)
            Assert.Equivalent([], rest)
    |   _ -> Assert.True(false)
    
[<Fact>]
let ``Operator: .`` () =
    match "." |> Seq.toList, 0u  with
    |   OperatorOrDelimiter(text, rest) ->
            Assert.Equal(Symbol.Period(0u,1u), text)
            Assert.Equivalent([], rest)
    |   _ -> Assert.True(false)
    
[<Fact>]
let ``Operator: matrices`` () =
    match "@" |> Seq.toList, 0u  with
    |   OperatorOrDelimiter(text, rest) ->
            Assert.Equal(Symbol.Matrices(0u,1u), text)
            Assert.Equivalent([], rest)
    |   _ -> Assert.True(false) 
     
[<Fact>]
let ``Operator: Not found`` () =
    match "56.78" |> Seq.toList, 0u  with
    |   OperatorOrDelimiter _ ->
            Assert.True(false)
    |   _ -> Assert.True(true)
    
(* Numbers *)

[<Fact>]
let ``Number: hex number`` () =
    match "0x7fb18" |> Seq.toList, 0u  with
    |   Number(text, rest) ->
            Assert.Equal(Symbol.Number(0u,7u, "0x7fb18"), text)
            Assert.Equivalent([], rest)
    |   _ -> Assert.True(false)
    
[<Fact>]
let ``Number: hex numbe with _`` () =
    match "0X_FF_7b_e" |> Seq.toList, 0u  with
    |   Number(text, rest) ->
            Assert.Equal(Symbol.Number(0u,10u, "0x_ff_7b_e"), text)
            Assert.Equivalent([], rest)
    |   _ -> Assert.True(false)
    
[<Fact>]
let ``Number: octet number`` () =
    match "0o7711" |> Seq.toList, 0u  with
    |   Number(text, rest) ->
            Assert.Equal(Symbol.Number(0u,6u, "0o7711"), text)
            Assert.Equivalent([], rest)
    |   _ -> Assert.True(false)
    
[<Fact>]
let ``Number: octet number with _`` () =
    match "0O_77_1_1" |> Seq.toList, 0u  with
    |   Number(text, rest) ->
            Assert.Equal(Symbol.Number(0u,9u, "0o_77_1_1"), text)
            Assert.Equivalent([], rest)
    |   _ -> Assert.True(false)
    
[<Fact>]
let ``Number: binary number`` () =
    match "0b110011" |> Seq.toList, 0u  with
    |   Number(text, rest) ->
            Assert.Equal(Symbol.Number(0u,8u, "0b110011"), text)
            Assert.Equivalent([], rest)
    |   _ -> Assert.True(false)
    
[<Fact>]
let ``Number: binary numbe with _`` () =
    match "0B_1_1_0_0_1_1" |> Seq.toList, 0u  with
    |   Number(text, rest) ->
            Assert.Equal(Symbol.Number(0u,14u, "0b_1_1_0_0_1_1"), text)
            Assert.Equivalent([], rest)
    |   _ -> Assert.True(false)
    
[<Fact>]
let ``Number: exponent simple`` () =
    match "e2_45" |> Seq.toList with
    | ExponentPart(text, rest) ->
            Assert.Equal("e2_45", text)
            Assert.Equivalent([], rest)
    |   _ -> Assert.True(false)
    
[<Fact>]
let ``Number: exponent signed`` () =
    match "e-2_45" |> Seq.toList with
    | ExponentPart(text, rest) ->
            Assert.Equal("e-2_45", text)
            Assert.Equivalent([], rest)
    |   _ -> Assert.True(false)
    
[<Fact>]
let ``Number: exponent signed and imaginary`` () =
    match "e-2_45J" |> Seq.toList with
    | ExponentPart(text, rest) ->
            Assert.Equal("e-2_45j", text)
            Assert.Equivalent([], rest)
    |   _ -> Assert.True(false)
    
[<Fact>]                                                                                
let ``Tokenizer: Number starting with period simple`` () =                                  
    let result = ".5" |> Seq.toList |> Tokenize |> Parse    
    Assert.Equivalent((AST.Number(0u, 2u, ".5"), []), result)
    
[<Fact>]                                                                                
let ``Tokenizer: Number starting with period simple with _`` () =                                  
    let result = ".5_1" |> Seq.toList |> Tokenize |> Parse    
    Assert.Equivalent((AST.Number(0u, 4u, ".5_1"), []), result)
    
[<Fact>]                                                                                
let ``Tokenizer: Number starting with period imaginary with _`` () =                                  
    let result = ".5_1_1_J" |> Seq.toList |> Tokenize |> Parse    
    Assert.Equivalent((AST.Number(0u, 8u, ".5_1_1_j"), []), result)
    
[<Fact>]                                                                                
let ``Tokenizer: Number starting with period exponent and imaginary with _`` () =                                  
    let result = ".5_1_1_e-3_4J" |> Seq.toList |> Tokenize |> Parse    
    Assert.Equivalent((AST.Number(0u, 13u, ".5_1_1_e-3_4j"), []), result)
    
[<Fact>]                                                                                
let ``Tokenizer: Number starting with nonzero simple`` () =                                  
    let result = "1.0" |> Seq.toList |> Tokenize |> Parse    
    Assert.Equivalent((AST.Number(0u, 3u, "1.0"), []), result)
    
[<Fact>]                                                                                
let ``Tokenizer: Number starting with nonzero simple with _`` () =                                  
    let result = "1_2.0" |> Seq.toList |> Tokenize |> Parse    
    Assert.Equivalent((AST.Number(0u, 5u, "1_2.0"), []), result)
    
[<Fact>]                                                                                
let ``Tokenizer: Number starting with nonzero`` () =                                  
    let result = "1_.0_3_5" |> Seq.toList |> Tokenize |> Parse    
    Assert.Equivalent((AST.Number(0u, 8u, "1_.0_3_5"), []), result)
    
[<Fact>]                                                                                
let ``Tokenizer: Number starting with nonzero imiginary`` () =                                  
    let result = "1_.0_3_5J" |> Seq.toList |> Tokenize |> Parse    
    Assert.Equivalent((AST.Number(0u, 9u, "1_.0_3_5j"), []), result)
    
[<Fact>]                                                                                
let ``Tokenizer: Number starting with nonzero exponent imiginary`` () =                                  
    let result = "1_.0_3_5E+4_5_J" |> Seq.toList |> Tokenize |> Parse    
    Assert.Equivalent((AST.Number(0u, 15u, "1_.0_3_5e+4_5_j"), []), result)
    
[<Fact>]                                                                                
let ``Tokenizer: Number starting with zeros`` () =                                  
    let result = "0_00" |> Seq.toList |> Tokenize |> Parse    
    Assert.Equivalent((AST.Number(0u, 4u, "0_00"), []), result)
    
[<Fact>]                                                                                
let ``Tokenizer: Number starting with zeros AND Imaginary`` () =                                  
    let result = "0_00J" |> Seq.toList |> Tokenize |> Parse    
    Assert.Equivalent((AST.Number(0u, 5u, "0_00j"), []), result)  
    
[<Fact>]                                                                                
let ``Tokenizer: Number starting with zeros and exponentImaginary`` () =                                  
    let result = "0_00e-34J" |> Seq.toList |> Tokenize |> Parse    
    Assert.Equivalent((AST.Number(0u, 9u, "0_00e-34j"), []), result)
    
[<Fact>]                                                                                
let ``Tokenizer: Number starting with zeros period and exponent Imaginary`` () =                                  
    let result = "0_00.5_6e-34J" |> Seq.toList |> Tokenize |> Parse    
    Assert.Equivalent((AST.Number(0u, 13u, "0_00.5_6e-34j"), []), result)
    
[<Fact>]                                                                                
let ``Tokenizer: Number 0.0j`` () =                                  
    let result = "0.0j" |> Seq.toList |> Tokenize |> Parse    
    Assert.Equivalent((AST.Number(0u, 4u, "0.0j"), []), result)
    
[<Fact>]                                                                                
let ``Tokenizer: String with single quote that is empty`` () =                                  
    let result = "'';" |> Seq.toList |> Tokenize |> Parse    
    Assert.Equivalent((AST.String(0u, 2u, [ "''" ]), [ Symbol.SemiColon(2u, 3u) ]), result)
    
[<Fact>]                                                                                
let ``Tokenizer: String with double quote that is empty`` () =                                  
    let result = "\"\";" |> Seq.toList |> Tokenize |> Parse    
    Assert.Equivalent((AST.String(0u, 2u, [ "\"\"" ]), [ Symbol.SemiColon(2u, 3u) ]), result)
    
[<Fact>]                                                                                
let ``Tokenizer: String with triple single quote that is empty`` () =                                  
    let result = "'''''';" |> Seq.toList |> Tokenize |> Parse    
    Assert.Equivalent((AST.String(0u, 6u, [ "''''''" ]), [ Symbol.SemiColon(6u, 7u) ]), result)
    
[<Fact>]                                                                                
let ``Tokenizer: String with triple double quote that is empty`` () =                                  
    let result = "\"\"\"\"\"\";" |> Seq.toList |> Tokenize |> Parse    
    Assert.Equivalent((AST.String(0u, 6u, [ "\"\"\"\"\"\"" ]), [ Symbol.SemiColon(6u, 7u) ]), result)
    
[<Fact>]                                                                                
let ``Tokenizer: String with single quote`` () =                                  
    let result = "'Hello, World!';" |> Seq.toList |> Tokenize |> Parse    
    Assert.Equivalent((AST.String(0u, 15u, [ "'Hello, World!'" ]), [ Symbol.SemiColon(15u, 16u) ]), result)
    
[<Fact>]                                                                                
let ``Tokenizer: String with double quote`` () =                                  
    let result = "\"Hello, World!\";" |> Seq.toList |> Tokenize |> Parse    
    Assert.Equal((AST.String(0u, 15u, [ "\"Hello, World!\"" ]), [ Symbol.SemiColon(15u, 16u); Symbol.EndOfFile 16u ]), result)
    
(* Expression parser unittests *)
       
[<Fact>]                                                                                
let ``Primary Expression rule: await expression2`` () =                                  
    let result = "await __init__" |> Seq.toList |> Tokenize |> Parse    
    Assert.Equivalent((AST.Await(0u, 0u, AST.Name(6u, 14u, "__init__")), [ Symbol.EndOfFile 14u  ]), result)   
    
[<Fact>]                                                                                                                                                                  
let ``Sum rule: plus and minus`` () =                                                                                                                                     
    let result = "a+b-c;" |> Seq.toList |> Tokenize |> Parse
    Assert.Equivalent((AST.Minus(0u, 5u, AST.Plus(0u, 3u, AST.Name(0u, 1u, "a"), AST.Name(2u, 3u, "b")), AST.Name(4u, 5u, "c")), [ Symbol.SemiColon(5u, 6u); Symbol.EndOfFile 6u ]), result) 
    
[<Fact>]
let ``Atom rule: strings`` () =
    let result = "'Hello, World!'r'Test';" |> Seq.toList |> Tokenize |> Parse
    Assert.Equivalent((AST.String(0u, 22u, ["'Hello, World!'"; "r'Test'"]), [ Symbol.SemiColon(22u, 23u); Symbol.EndOfFile 23u ]), result)
    
[<Fact>]
let ``Atom rule: raw strings`` () =
    let result = "r'Test';" |> Seq.toList |> Tokenize |> Parse
    Assert.Equal((AST.String(0u, 7u, ["r'Test'"]), [ Symbol.SemiColon(7u, 8u); Symbol.EndOfFile 8u ]), result)
    
    
[<Fact>]
let ``Lambda without arguments`` () =
    let result = "lambda: a + 1;" |> Seq.toList |> Tokenize |> Parse
    Assert.Equal((AST.Lambda(0u, 14u, AST.Empty, AST.Plus(8u, 13u, AST.Name(8u, 9u, "a"), AST.Number(12u, 13u, "1"))), [ Symbol.SemiColon(13u, 14u); Symbol.EndOfFile 14u ]), result)
    
[<Fact>]
let ``Empty Set or Dictionary`` () =
    let result = "{};" |> Seq.toList |> Tokenize |> Parse
    Assert.Equal((AST.EmptySetOrDictionary(0u, 2u), [ Symbol.SemiColon(2u, 3u); Symbol.EndOfFile 3u ]), result)
    
[<Fact>]
let ``Set simple`` () =
    let result = "{ a, };" |> Seq.toList |> Tokenize |> Parse
    Assert.Equal((AST.Set(0u, 6u, [ AST.Name(2u, 3u, "a") ]), [ Symbol.SemiColon(6u, 7u); Symbol.EndOfFile 7u ]), result)
    
[<Fact>]
let ``Set multiple`` () =
    let result = "{ a, b, };" |> Seq.toList |> Tokenize |> Parse
    Assert.Equal((AST.Set(0u, 9u, [ AST.Name(2u, 3u, "a"); AST.Name(5u, 6u, "b") ]), [ Symbol.SemiColon(9u, 10u); Symbol.EndOfFile 10u ]), result)
      
[<Fact>]
let ``Dictionary simple`` () =
    let result = "{ a : 1, };" |> Seq.toList |> Tokenize |> Parse
    Assert.Equal((AST.Dictionary(0u, 10u, [
        AST.DictionaryKeyValue(2u, 7u, AST.Name(2u, 3u, "a"), AST.Number(6u, 7u, "1"))
    ]), [ Symbol.SemiColon(10u, 11u); Symbol.EndOfFile 11u ]), result)
    
[<Fact>]
let ``Dictionary multiple`` () =
    let result = "{ a : 1, b : 2, };" |> Seq.toList |> Tokenize |> Parse
    Assert.Equal((AST.Dictionary(0u, 17u, [
        AST.DictionaryKeyValue(2u, 7u, AST.Name(2u, 3u, "a"), AST.Number(6u, 7u, "1"))
        AST.DictionaryKeyValue(9u, 14u, AST.Name(9u, 10u, "b"), AST.Number(13u, 14u, "2"))
    ]), [ Symbol.SemiColon(17u, 18u); Symbol.EndOfFile 18u ]), result)
    
[<Fact>]
let ``Dictionary multiple with power`` () =
    let result = "{ a : 1, **b, };" |> Seq.toList |> Tokenize |> Parse
    Assert.Equal((AST.Dictionary(0u, 15u, [
        AST.DictionaryKeyValue(2u, 7u, AST.Name(2u, 3u, "a"), AST.Number(6u, 7u, "1"))
        AST.DictionaryFromDictionary(9u, 12u, AST.Name(11u, 12u, "b"))
    ]), [ Symbol.SemiColon(15u, 16u); Symbol.EndOfFile 16u ]), result)
    
[<Fact>]
let ``Empty tuple`` () =
    let result = "();" |> Seq.toList |> Tokenize |> Parse
    Assert.Equal((AST.Tuple(0u, 2u, []), [ Symbol.SemiColon(2u, 3u); Symbol.EndOfFile 3u ]), result)
    
[<Fact>]
let ``Empty list`` () =
    let result = "[];" |> Seq.toList |> Tokenize |> Parse
    Assert.Equal((AST.List(0u, 2u, []), [ Symbol.SemiColon(2u, 3u); Symbol.EndOfFile 3u ]), result)
    
    
(* Statement pattern tests *)

[<Fact>]
let ``Statement: Pass statement`` () =
    let result = "pass" |> Seq.toList |> Tokenize |> ParseFromFile
    Assert.Equal((AST.PassStatement(0u, 4u), [ ]), result)

[<Fact>]
let ``Statement: Empty raise statement`` () =
    let result = "raise; pass" |> Seq.toList |> Tokenize |> ParseFromFile
    Assert.Equal(( AST.SimpleStatementList(0u, 11u, [ AST.RaiseStatement(0u, 5u, AST.Empty, AST.Empty); AST.PassStatement(7u, 11u) ]), [ ]), result)
    
[<Fact>]
let ``Statement: Empty raise with one argument statement`` () =
    let result = "raise a" |> Seq.toList |> Tokenize |> ParseFromFile
    Assert.Equal((AST.RaiseStatement(0u, 7u, AST.Name(6u, 7u, "a"), AST.Empty), [ ]), result)
    
[<Fact>]
let ``Statement: Empty raise with two argument statement`` () =
    let result = "raise a from b" |> Seq.toList |> Tokenize |> ParseFromFile
    Assert.Equal((AST.RaiseStatement(0u, 14u, AST.Name(6u, 7u, "a"), AST.Name(13u, 14u, "b")), [ ]), result)
    
[<Fact>]
let ``Statement: Empty assert with one argument statement`` () =
    let result = "assert a" |> Seq.toList |> Tokenize |> ParseFromFile
    Assert.Equal((AST.AssertStatement(0u, 8u, AST.Name(7u, 8u, "a"), AST.Empty), [ ]), result)
    
[<Fact>]
let ``Statement: Empty assert with two argument statement`` () =
    let result = "assert a, b" |> Seq.toList |> Tokenize |> ParseFromFile
    Assert.Equal((AST.AssertStatement(0u, 11u, AST.Name(7u, 8u, "a"), AST.Name(10u, 11u, "b")), [ ]), result)
    
[<Fact>]
let ``Statement: break statement`` () =
    let result = "break" |> Seq.toList |> Tokenize |> ParseFromFile
    Assert.Equal((AST.BreakStatement(0u, 5u), [ ]), result)
    
[<Fact>]
let ``Statement: continue statement`` () =
    let result = "continue" |> Seq.toList |> Tokenize |> ParseFromFile
    Assert.Equal((AST.ContinueStatement(0u, 8u), [ ]), result)
    
[<Fact>]
let ``Statement: global statement with one element`` () =
    let result = "global a" |> Seq.toList |> Tokenize |> ParseFromFile
    Assert.Equal((AST.GlobalStatement(0u, 8u, [ AST.Name(7u, 8u, "a") ]), [ ]), result)
    
[<Fact>]
let ``Statement: global statement with two element`` () =
    let result = "global a,b" |> Seq.toList |> Tokenize |> ParseFromFile
    Assert.Equal((AST.GlobalStatement(0u, 10u, [ AST.Name(7u, 8u, "a"); AST.Name(9u, 10u, "b") ]), [ ]), result)
    
[<Fact>]
let ``Statement: return statement without element`` () =
    let result = "return; pass" |> Seq.toList |> Tokenize |> ParseFromFile
    Assert.Equal(( AST.SimpleStatementList(0u, 12u, [ AST.ReturnStatement(0u, 6u, AST.Empty); AST.PassStatement(8u, 12u) ]), [ ]), result)
    
[<Fact>]
let ``Statement: return statement with one element`` () =
    let result = "return a" |> Seq.toList |> Tokenize |> ParseFromFile
    Assert.Equal((AST.ReturnStatement(0u, 8u, AST.Name(7u, 8u, "a") ), [ ]), result)
    
[<Fact>]
let ``Statement: if statement with one element`` () =
    let result = "if a > 1: pass" |> Seq.toList |> Tokenize |> ParseFromFile
    Assert.Equal((AST.IfBlock(0u, 14u, AST.Greater(3u, 8u, AST.Name(3u, 4u, "a"), AST.Number(7u, 8u, "1")), AST.PassStatement(10u, 14u), [], AST.Empty), [ ]), result)
    
[<Fact>]
let ``Statement: if/else statement with one element`` () =
    let result = "if a > 1: pass\nelse: pass" |> Seq.toList |> Tokenize |> ParseFromFile
    Assert.Equal((AST.IfBlock(0u, 25u, AST.Greater(3u, 8u, AST.Name(3u, 4u, "a"), AST.Number(7u, 8u, "1")), AST.PassStatement(10u, 15u), [], AST.ElseBlock(15u, 25u, AST.PassStatement(21u, 25u))), [ ]), result)
    
[<Fact>]
let ``Statement: if/elif/else statement with one element`` () =
    let result = "if a > 1: pass\nelif a == 0: pass\nelse: pass" |> Seq.toList |> Tokenize |> ParseFromFile
    Assert.Equivalent((AST.IfBlock(0u, 43u, AST.Greater(3u, 8u, AST.Name(3u, 4u, "a"), AST.Number(7u, 8u, "1")), AST.PassStatement(10u, 15u),
                              [ AST.ElifBlock(15u, 33u, AST.Equal(20u, 26u, AST.Name(20u, 21u, "a"), AST.Number(25u, 26u, "0")), AST.PassStatement(28u, 33u)) ],
                              AST.ElseBlock(33u, 43u, AST.PassStatement(39u, 43u))), [ ]), result)