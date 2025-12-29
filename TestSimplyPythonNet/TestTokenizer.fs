module TestTokenizer

open System
open System.Linq
open Xunit
open SimplyPythonNet.tokenizer

[<Fact>]
let ``Reserved keyword: None`` () = Assert.Equivalent(Some(Token.None), ReservedKeyword "None")

[<Fact>]
let ``Reserved keyword: True`` () = Assert.Equivalent(Some(Token.True), ReservedKeyword "True")

[<Fact>]
let ``Reserved keyword: False`` () = Assert.Equivalent(Some(Token.False), ReservedKeyword "False")

[<Fact>]
let ``Reserved keyword: and`` () = Assert.Equivalent(Some(Token.And), ReservedKeyword "and")

[<Fact>]
let ``Reserved keyword: as`` () = Assert.Equivalent(Some(Token.As), ReservedKeyword "as")

[<Fact>]
let ``Reserved keyword: assert`` () = Assert.Equivalent(Some(Token.Assert), ReservedKeyword "assert")

[<Fact>]
let ``Reserved keyword: async`` () = Assert.Equivalent(Some(Token.Async), ReservedKeyword "async")

[<Fact>]
let ``Reserved keyword: await`` () = Assert.Equivalent(Some(Token.Await), ReservedKeyword "await")

[<Fact>]
let ``Reserved keyword: break`` () = Assert.Equivalent(Some(Token.Break), ReservedKeyword "break")

[<Fact>]
let ``Reserved keyword: continue`` () = Assert.Equivalent(Some(Token.None), ReservedKeyword "continue")

[<Fact>]
let ``Reserved keyword: class`` () = Assert.Equivalent(Some(Token.Class), ReservedKeyword "class")

[<Fact>]
let ``Reserved keyword: def`` () = Assert.Equivalent(Some(Token.Def), ReservedKeyword "def")

[<Fact>]
let ``Reserved keyword: del`` () = Assert.Equivalent(Some(Token.Del), ReservedKeyword "del")

[<Fact>]
let ``Reserved keyword: elif`` () = Assert.Equivalent(Some(Token.Elif), ReservedKeyword "elif")

[<Fact>]
let ``Reserved keyword: else`` () = Assert.Equivalent(Some(Token.Else), ReservedKeyword "else")

[<Fact>]
let ``Reserved keyword: except`` () = Assert.Equivalent(Some(Token.Except), ReservedKeyword "except")

[<Fact>]
let ``Reserved keyword: for`` () = Assert.Equivalent(Some(Token.For), ReservedKeyword "for")

[<Fact>]
let ``Reserved keyword: finally`` () = Assert.Equivalent(Some(Token.Finally), ReservedKeyword "finally")

[<Fact>]
let ``Reserved keyword: from`` () = Assert.Equivalent(Some(Token.From), ReservedKeyword "from")

[<Fact>]
let ``Reserved keyword: global`` () = Assert.Equivalent(Some(Token.Global), ReservedKeyword "global")

[<Fact>]
let ``Reserved keyword: if`` () = Assert.Equivalent(Some(Token.If), ReservedKeyword "if")

[<Fact>]
let ``Reserved keyword: in`` () = Assert.Equivalent(Some(Token.In), ReservedKeyword "in")

[<Fact>]
let ``Reserved keyword: is`` () = Assert.Equivalent(Some(Token.Is), ReservedKeyword "is")

[<Fact>]
let ``Reserved keyword: lambda`` () = Assert.Equivalent(Some(Token.Lambda), ReservedKeyword "lambda")

[<Fact>]
let ``Reserved keyword: nonlocal`` () = Assert.Equivalent(Some(Token.Nonlocal), ReservedKeyword "nonlocal")

[<Fact>]
let ``Reserved keyword: not`` () = Assert.Equivalent(Some(Token.Not), ReservedKeyword "not")

[<Fact>]
let ``Reserved keyword: or`` () = Assert.Equivalent(Some(Token.Or), ReservedKeyword "or")

[<Fact>]
let ``Reserved keyword: pass`` () = Assert.Equivalent(Some(Token.Pass), ReservedKeyword "pass")

[<Fact>]
let ``Reserved keyword: raise`` () = Assert.Equivalent(Some(Token.Raise), ReservedKeyword "raise")

[<Fact>]
let ``Reserved keyword: return`` () = Assert.Equivalent(Some(Token.Return), ReservedKeyword "return")

[<Fact>]
let ``Reserved keyword: try`` () = Assert.Equivalent(Some(Token.Try), ReservedKeyword "try")

[<Fact>]
let ``Reserved keyword: while`` () = Assert.Equivalent(Some(Token.While), ReservedKeyword "while")

[<Fact>]
let ``Reserved keyword: with`` () = Assert.Equivalent(Some(Token.With), ReservedKeyword "with")

[<Fact>]
let ``Reserved keyword: yield`` () = Assert.Equivalent(Some(Token.Yield), ReservedKeyword "yield")

[<Fact>]
let ``Reserved keyword: Not Found`` () = Assert.Equal(Option.None, ReservedKeyword "match")

[<Fact>]
let ``Single character operator: &`` () = Assert.Equivalent(Some(Token.BitwiseAnd), OneCharToken '&')

[<Fact>]
let ``Single character operator: |`` () = Assert.Equivalent(Some(Token.BitwiseOr), OneCharToken '|')

[<Fact>]
let ``Single character operator: ^`` () = Assert.Equivalent(Some(Token.BitwiseXor), OneCharToken '^')

[<Fact>]
let ``Single character operator: ~`` () = Assert.Equivalent(Some(Token.BitwiseInvert), OneCharToken '~')

[<Fact>]
let ``Single character operator: <`` () = Assert.Equivalent(Some(Token.Less), OneCharToken '<')

[<Fact>]
let ``Single character operator: >`` () = Assert.Equivalent(Some(Token.Greater), OneCharToken '>')

[<Fact>]
let ``Single character operator: (`` () = Assert.Equivalent(Some(Token.LeftParen), OneCharToken '(')

[<Fact>]
let ``Single character operator: )`` () = Assert.Equivalent(Some(Token.RightParen), OneCharToken ')')

[<Fact>]
let ``Single character operator: [`` () = Assert.Equivalent(Some(Token.LeftBracket), OneCharToken '[')

[<Fact>]
let ``Single character operator: ]`` () = Assert.Equivalent(Some(Token.RightBracket), OneCharToken ']')

[<Fact>]
let ``Single character operator: {`` () = Assert.Equivalent(Some(Token.LeftCurly), OneCharToken '{')

[<Fact>]
let ``Single character operator: }`` () = Assert.Equivalent(Some(Token.RightCurly), OneCharToken '}')

[<Fact>]
let ``Single character operator: ,`` () = Assert.Equivalent(Some(Token.Comma), OneCharToken ',')

[<Fact>]
let ``Single character operator: :`` () = Assert.Equivalent(Some(Token.Colon), OneCharToken ':')

[<Fact>]
let ``Single character operator: !`` () = Assert.Equivalent(Some(Token.BitwiseNot), OneCharToken '!')

[<Fact>]
let ``Single character operator: ;`` () = Assert.Equivalent(Some(Token.SemiColon), OneCharToken ';')

[<Fact>]
let ``Single character operator: =`` () = Assert.Equivalent(Some(Token.Assign), OneCharToken '=')

[<Fact>]
let ``Single character operator: +`` () = Assert.Equivalent(Some(Token.Plus), OneCharToken '+')

[<Fact>]
let ``Single character operator: -`` () = Assert.Equivalent(Some(Token.Minus), OneCharToken '-')

[<Fact>]
let ``Single character operator: *`` () = Assert.Equivalent(Some(Token.Mul), OneCharToken '*')

[<Fact>]
let ``Single character operator: /`` () = Assert.Equivalent(Some(Token.Slash), OneCharToken '/')

[<Fact>]
let ``Single character operator: %`` () = Assert.Equivalent(Some(Token.Modulo), OneCharToken '%')

[<Fact>]
let ``Single character operator: .`` () = Assert.Equivalent(Some(Token.Period), OneCharToken '.')

[<Fact>]
let ``Single character operator: Matrices`` () = Assert.Equivalent(Some(Token.Matrices), OneCharToken '@')

[<Fact>]
let ``Double character operator: +=`` () = Assert.Equivalent(Some(Token.PlusAssign), TwoCharToken('+', '='))

[<Fact>]
let ``Double character operator: -=`` () = Assert.Equivalent(Some(Token.MinusAssign), TwoCharToken('-', '='))

[<Fact>]
let ``Double character operator: *=`` () = Assert.Equivalent(Some(Token.MulAssign), TwoCharToken('*', '='))

[<Fact>]
let ``Double character operator: /=`` () = Assert.Equivalent(Some(Token.SlashAssign), TwoCharToken('/', '='))

[<Fact>]
let ``Double character operator: %=`` () = Assert.Equivalent(Some(Token.MinusAssign), TwoCharToken('%', '='))

[<Fact>]
let ``Double character operator: &=`` () = Assert.Equivalent(Some(Token.BitwiseAndAssign), TwoCharToken('&', '='))

[<Fact>]
let ``Double character operator: |=`` () = Assert.Equivalent(Some(Token.BitwiseOrAssign), TwoCharToken('|', '='))

[<Fact>]
let ``Double character operator: ^=`` () = Assert.Equivalent(Some(Token.BitwiseXorAssign), TwoCharToken('^', '='))

[<Fact>]
let ``Double character operator: Matrices =`` () = Assert.Equivalent(Some(Token.MatricesAssign), TwoCharToken('@', '='))

[<Fact>]
let ``Double character operator: :=`` () = Assert.Equivalent(Some(Token.ColonAssign), TwoCharToken(':', '='))

[<Fact>]
let ``Double character operator: <<`` () = Assert.Equivalent(Some(Token.BitwiseShiftLeft), TwoCharToken('<', '<'))

[<Fact>]
let ``Double character operator: >>`` () = Assert.Equivalent(Some(Token.BitwiseShiftRight), TwoCharToken('>', '>'))

[<Fact>]
let ``Double character operator: <=`` () = Assert.Equivalent(Some(Token.LessEqual), TwoCharToken('<', '='))

[<Fact>]
let ``Double character operator: >=`` () = Assert.Equivalent(Some(Token.GreaterEqual), TwoCharToken('>', '='))

[<Fact>]
let ``Double character operator: ==`` () = Assert.Equivalent(Some(Token.Equal), TwoCharToken('=', '='))

[<Fact>]
let ``Double character operator: !=`` () = Assert.Equivalent(Some(Token.NotEqual), TwoCharToken('!', '='))

[<Fact>]
let ``Double character operator: ->`` () = Assert.Equivalent(Some(Token.Arrow), TwoCharToken('-', '>'))

[<Fact>]
let ``Double character operator: **`` () = Assert.Equivalent(Some(Token.Power), TwoCharToken('*', '*'))

[<Fact>]
let ``Double character operator: //`` () = Assert.Equivalent(Some(Token.DoubleSlash), TwoCharToken('/', '/'))

[<Fact>]
let ``Triple character operator: <<=`` () = Assert.Equivalent(Some(Token.BitwiseShiftLeftAssign), ThreeCharToken('<', '<', '='))

[<Fact>]
let ``Triple character operator: >>=`` () = Assert.Equivalent(Some(Token.BitwiseShiftRightAssign), ThreeCharToken('>', '>', '='))

[<Fact>]
let ``Triple character operator: **=`` () = Assert.Equivalent(Some(Token.PowerAssign), ThreeCharToken('*', '*', '='))

[<Fact>]
let ``Triple character operator: //=`` () = Assert.Equivalent(Some(Token.DoubleSlashAssign), ThreeCharToken('/', '/', '='))

[<Fact>]
let ``Triple character operator: ...`` () = Assert.Equivalent(Some(Token.Ellipsis), ThreeCharToken('.', '.', '.'))

[<Fact>]
let ``Soft keyword: match`` () = Assert.Equivalent(Some(Token.Match(1u,1u)), SoftKeyword (Token.Name( 1u, 1u, "match")))

[<Fact>]
let ``Soft keyword: case`` () = Assert.Equivalent(Some(Token.Case(1u,1u)), SoftKeyword (Token.Name( 1u, 1u, "case")))

[<Fact>]
let ``Soft keyword: _`` () = Assert.Equivalent(Some(Token.Default(1u,1u)), SoftKeyword (Token.Name( 1u, 1u, "_")))

[<Fact>]
let ``Soft keyword: type`` () = Assert.Equivalent(Some(Token.Type(1u,1u)), SoftKeyword (Token.Name( 1u, 1u, "type")))

[<Fact>]
let ``PeekNextThreeChars: Tripple`` () = Assert.Equal(('*', '*', '='), PeekNextThreeChars ("**=" |> Seq.toList))

[<Fact>]
let ``PeekNextThreeChars: Double`` () = Assert.Equal(('*', '*', '\u0000'), PeekNextThreeChars ("**" |> Seq.toList))

[<Fact>]
let ``PeekNextThreeChars: Single`` () = Assert.Equal(('*', '\u0000', '\u0000'), PeekNextThreeChars ("*" |> Seq.toList))

[<Fact>]
let ``PeekNextThreeChars: Zero`` () = Assert.Equal(('\u0000', '\u0000', '\u0000'), PeekNextThreeChars ("" |> Seq.toList))

[<Fact>]
let ``PeekNextTwoChars: Two`` () = Assert.Equal(('<', '<'), PeekNextTwoChars ("<<" |> Seq.toList))

[<Fact>]
let ``PeekNextTwoChars: One`` () = Assert.Equal(('<', '\u0000'), PeekNextTwoChars ("<" |> Seq.toList))

[<Fact>]
let ``PeekNextTwoChars: Zero`` () = Assert.Equal(('\u0000', '\u0000'), PeekNextTwoChars ("" |> Seq.toList))

[<Fact>]
let ``PeekNextChar: One`` () = Assert.Equal('<', PeekNextChar ("<" |> Seq.toList))

[<Fact>]
let ``PeekNextChar: Zero`` () = Assert.Equal('\u0000', PeekNextChar ("" |> Seq.toList))