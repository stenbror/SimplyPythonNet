

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
    
(* Expression parser unittests *)
       
[<Fact>]                                                                                
let ``Primary Expression rule: await expression2`` () =                                  
    let result = "await __init__" |> Seq.toList |> Tokenize |> Parse    
    Assert.Equivalent((AST.Await(0u, 0u, AST.Name(6u, 14u, "__init__")), []), result)   
    
[<Fact>]                                                                                                                                                                  
let ``Sum rule: plus and minus`` () =                                                                                                                                     
    let result = "a+b-c;" |> Seq.toList |> Tokenize |> Parse
    Assert.Equivalent((AST.Minus(0u, 5u, AST.Plus(0u, 3u, AST.Name(0u, 1u, "a"), AST.Name(2u, 3u, "b")), AST.Name(4u, 5u, "c")), [ Symbol.SemiColon(5u, 6u) ]), result) 
    
[<Fact>]
let ``Atom rule: strings`` () =
    let result = [ Symbol.String(0u, 22u, "'Hello, World!'"); Symbol.String(0u, 1u, "r'Test'"); Symbol.SemiColon(22u, 23u) ] |> Parse
    Assert.Equivalent((AST.String(0u, 22u, ["'Hello, World!'"; "r'Test'"]), [ Symbol.SemiColon(22u, 23u) ]), result)