module TestTokenizer

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
let ``Soft keyword: match`` () = Assert.Equivalent(Some(Token.Match(1u,1u)), SoftKeyword (Token.NameLiteral( 1u, 1u, "match")))

[<Fact>]
let ``Soft keyword: case`` () = Assert.Equivalent(Some(Token.Case(1u,1u)), SoftKeyword (Token.NameLiteral( 1u, 1u, "case")))

[<Fact>]
let ``Soft keyword: _`` () = Assert.Equivalent(Some(Token.Default(1u,1u)), SoftKeyword (Token.NameLiteral( 1u, 1u, "_")))

[<Fact>]
let ``Soft keyword: type`` () = Assert.Equivalent(Some(Token.Type(1u,1u)), SoftKeyword (Token.NameLiteral( 1u, 1u, "type")))

[<Fact>]
let ``PeekNextThreeChars: triple`` () = Assert.Equal(('*', '*', '='), PeekNextThreeChars ("**=" |> Seq.toList))

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

[<Fact>]
let ``AdvanceCharacters: 3`` () = Assert.Equal<char list>(("d" |> Seq.toList), AdvanceCharacters (("abcd" |> Seq.toList), 3u))

[<Fact>]
let ``AdvanceCharacters: 2`` () = Assert.Equal<char list>(("c" |> Seq.toList), AdvanceCharacters (("abc" |> Seq.toList), 2u))

[<Fact>]
let ``AdvanceCharacters: 1`` () = Assert.Equal<char list>(("b" |> Seq.toList), AdvanceCharacters (("ab" |> Seq.toList), 1u))

[<Fact>]
let ``AdvanceCharacters: 0`` () = Assert.Equal<char list>(("abc" |> Seq.toList), AdvanceCharacters (("abc" |> Seq.toList), 0u))

[<Fact>]
let ``Operator: **=`` () = 
    let res, rest = Operators("**= 4" |> Seq.toList)
    Assert.Equivalent(Some(Token.PowerAssign), res)
    Assert.Equal<char list>((" 4" |> Seq.toList), rest)

[<Fact>]
let ``Operator: **`` () = 
    let res, rest = Operators("** 4" |> Seq.toList)
    Assert.Equivalent(Some(Token.Power), res)
    Assert.Equal<char list>((" 4" |> Seq.toList), rest)
    
[<Fact>]
let ``Operator: *`` () = 
    let res, rest = Operators("* 4" |> Seq.toList)
    Assert.Equivalent(Some(Token.Mul), res)
    Assert.Equal<char list>((" 4" |> Seq.toList), rest)
    
[<Fact>]
let ``Operator: None`` () = 
    let res, rest = Operators(" 4" |> Seq.toList)
    Assert.Equivalent(Option.None, res)
    Assert.Equal<char list>((" 4" |> Seq.toList), rest)
    
    
[<Fact>]
let ``NextSymbol: None`` () = 
    let symbol, text, rest = NextSymbol ("None" |> Seq.toList)
    Assert.Equivalent(Some(Token.None), symbol)
    Assert.Equivalent(Option.None, text)
    Assert.Equal<char list>(("" |> Seq.toList), rest)
    
[<Fact>]
let ``NextSymbol: __name__`` () = 
    let symbol, text, rest = NextSymbol ("init__" |> Seq.toList)
    Assert.Equivalent(Some(Token.Name), symbol)
    Assert.Equivalent(Some("init__"), text)
    Assert.Equal<char list>(("" |> Seq.toList), rest)
    
[<Fact>]
let ``NextSymbol: __name1__`` () = 
    let symbol, text, rest = NextSymbol ("__name1__" |> Seq.toList)
    Assert.Equivalent(Some(Token.Name), symbol)
    Assert.Equivalent(Some("__name1__"), text)
    Assert.Equal<char list>(("" |> Seq.toList), rest)
    
[<Fact>]
let ``NextSymbol: <<=`` () = 
    let symbol, text, rest = NextSymbol ("<<=" |> Seq.toList)
    Assert.Equivalent(Some(Token.None), symbol)
    Assert.Equivalent(Option.None, text)
    Assert.Equal<char list>(("" |> Seq.toList), rest)
    
[<Fact>]
let ``NextSymbol: ...`` () = 
    let symbol, text, rest = NextSymbol ("..." |> Seq.toList)
    Assert.Equivalent(Some(Token.Ellipsis), symbol)
    Assert.Equivalent(Option.None, text)
    Assert.Equal<char list>(("" |> Seq.toList), rest)
    
[<Fact>]
let ``NextSymbol: .`` () = 
    let symbol, text, rest = NextSymbol (".test" |> Seq.toList)
    Assert.Equivalent(Some(Token.Ellipsis), symbol)
    Assert.Equivalent(Option.None, text)
    Assert.Equal<char list>(("test" |> Seq.toList), rest)
    
[<Fact>]
let ``NextSymbol: Fraction simple`` () = 
    let symbol, text, rest = NextSymbol (".45" |> Seq.toList)
    Assert.Equivalent(Some(Token.Number), symbol)
    Assert.Equivalent(Some(".45"), text)
    Assert.Equal<char list>(("" |> Seq.toList), rest)
    
[<Fact>]
let ``NextSymbol: Fraction simple with imaginary`` () = 
    let symbol, text, rest = NextSymbol (".45j" |> Seq.toList)
    Assert.Equivalent(Some(Token.Number), symbol)
    Assert.Equivalent(Some(".45j"), text)
    Assert.Equal<char list>(("" |> Seq.toList), rest)
    
[<Fact>]
let ``NextSymbol: Fraction simple with exponent`` () = 
    let symbol, text, rest = NextSymbol (".45e4" |> Seq.toList)
    Assert.Equivalent(Some(Token.Number), symbol)
    Assert.Equivalent(Some(".45e4"), text)
    Assert.Equal<char list>(("" |> Seq.toList), rest)
    
[<Fact>]
let ``NextSymbol: Fraction simple with exponent plus`` () = 
    let symbol, text, rest = NextSymbol (".45e+4" |> Seq.toList)
    Assert.Equivalent(Some(Token.Number), symbol)
    Assert.Equivalent(Some(".45e+4"), text)
    Assert.Equal<char list>(("" |> Seq.toList), rest)
    
[<Fact>]
let ``NextSymbol: Fraction simple with exponent minus`` () = 
    let symbol, text, rest = NextSymbol (".45e-4" |> Seq.toList)
    Assert.Equivalent(Some(Token.Number), symbol)
    Assert.Equivalent(Some(".45e-4"), text)
    Assert.Equal<char list>(("" |> Seq.toList), rest)
    
[<Fact>]
let ``NextSymbol: Fraction simple with exponent plus and imaginary`` () = 
    let symbol, text, rest = NextSymbol (".45e+4J" |> Seq.toList)
    Assert.Equivalent(Some(Token.Number), symbol)
    Assert.Equivalent(Some(".45e+4J"), text)
    Assert.Equal<char list>(("" |> Seq.toList), rest)
    
[<Fact>]
let ``NextSymbol: Fraction simple with exponent plus and imaginary and separators`` () = 
    let symbol, text, rest = NextSymbol (".4_5e+4_6J" |> Seq.toList)
    Assert.Equivalent(Some(Token.Number), symbol)
    Assert.Equivalent(Some(".45e+46J"), text)
    Assert.Equal<char list>(("" |> Seq.toList), rest)
    
[<Fact>]
let ``NextSymbol: Simple_hex_number`` () = 
    let symbol, text, rest = NextSymbol ("0X7F" |> Seq.toList)
    Assert.Equivalent(Some(Token.Number), symbol)
    Assert.Equivalent(Some("0x7F"), text)
    Assert.Equal<char list>(("" |> Seq.toList), rest)
    
[<Fact>]
let ``NextSymbol: Simple_hex_number_with_separators`` () = 
    let symbol, text, rest = NextSymbol ("0x_7_F" |> Seq.toList)
    Assert.Equivalent(Some(Token.Number), symbol)
    Assert.Equivalent(Some("0x7F"), text)
    Assert.Equal<char list>(("" |> Seq.toList), rest)
    
[<Fact>]
let ``NextSymbol: Simple_oct_number`` () = 
    let symbol, text, rest = NextSymbol ("0o7" |> Seq.toList)
    Assert.Equivalent(Some(Token.Number), symbol)
    Assert.Equivalent(Some("0o7"), text)
    Assert.Equal<char list>(("" |> Seq.toList), rest)
    
[<Fact>]
let ``NextSymbol: Simple_oct_number_with_separators`` () = 
    let symbol, text, rest = NextSymbol ("0O_7_5" |> Seq.toList)
    Assert.Equivalent(Some(Token.Number), symbol)
    Assert.Equivalent(Some("0o75"), text)
    Assert.Equal<char list>(("" |> Seq.toList), rest)
    
[<Fact>]
let ``NextSymbol: Simple_binary_number`` () = 
    let symbol, text, rest = NextSymbol ("0b10" |> Seq.toList)
    Assert.Equivalent(Some(Token.Number), symbol)
    Assert.Equivalent(Some("0b10"), text)
    Assert.Equal<char list>(("" |> Seq.toList), rest)
    
[<Fact>]
let ``NextSymbol: Simple_binary_number_with_separators`` () = 
    let symbol, text, rest = NextSymbol ("0B_1_0" |> Seq.toList)
    Assert.Equivalent(Some(Token.Number), symbol)
    Assert.Equivalent(Some("0b10"), text)
    Assert.Equal<char list>(("" |> Seq.toList), rest)
    
[<Fact>]
let ``NextSymbol: zero`` () = 
    let symbol, text, rest = NextSymbol ("0" |> Seq.toList)
    Assert.Equivalent(Some(Token.Number), symbol)
    Assert.Equivalent(Some("0"), text)
    Assert.Equal<char list>(("" |> Seq.toList), rest)
    
[<Fact>]
let ``NextSymbol: zero point zero imaginary`` () = 
    let symbol, text, rest = NextSymbol ("0.0j" |> Seq.toList)
    Assert.Equivalent(Some(Token.Number), symbol)
    Assert.Equivalent(Some("0.0j"), text)
    Assert.Equal<char list>(("" |> Seq.toList), rest)
    
[<Fact>]
let ``NextSymbol: zero exponent`` () = 
    let symbol, text, rest = NextSymbol ("0e-4" |> Seq.toList)
    Assert.Equivalent(Some(Token.Number), symbol)
    Assert.Equivalent(Some("0e-4"), text)
    Assert.Equal<char list>(("" |> Seq.toList), rest)
    
[<Fact>]
let ``NextSymbol: multiple zero exponent`` () = 
    let symbol, text, rest = NextSymbol ("00000e-4" |> Seq.toList)
    Assert.Equivalent(Some(Token.Number), symbol)
    Assert.Equivalent(Some("0e-4"), text)
    Assert.Equal<char list>(("" |> Seq.toList), rest)
    
    
[<Fact>]
let ``NextSymbol: simple integer non zero`` () = 
    let symbol, text, rest = NextSymbol ("1" |> Seq.toList)
    Assert.Equivalent(Some(Token.Number), symbol)
    Assert.Equivalent(Some("1"), text)
    Assert.Equal<char list>(("" |> Seq.toList), rest)
    
[<Fact>]
let ``NextSymbol: simple integer non zero nand separators`` () = 
    let symbol, text, rest = NextSymbol ("1_2_3_45_6" |> Seq.toList)
    Assert.Equivalent(Some(Token.Number), symbol)
    Assert.Equivalent(Some("123456"), text)
    Assert.Equal<char list>(("" |> Seq.toList), rest)
    
[<Fact>]
let ``NextSymbol: simple float non zero`` () = 
    let symbol, text, rest = NextSymbol ("1.1" |> Seq.toList)
    Assert.Equivalent(Some(Token.Number), symbol)
    Assert.Equivalent(Some("1.1"), text)
    Assert.Equal<char list>(("" |> Seq.toList), rest)
    
[<Fact>]
let ``NextSymbol: simple float non zero with imaginary`` () = 
    let symbol, text, rest = NextSymbol ("1.1J" |> Seq.toList)
    Assert.Equivalent(Some(Token.Number), symbol)
    Assert.Equivalent(Some("1.1J"), text)
    Assert.Equal<char list>(("" |> Seq.toList), rest)
    
[<Fact>]
let ``NextSymbol: simple float non zero exponent`` () = 
    let symbol, text, rest = NextSymbol ("1.1e-1" |> Seq.toList)
    Assert.Equivalent(Some(Token.Number), symbol)
    Assert.Equivalent(Some("1.1e-1"), text)
    Assert.Equal<char list>(("" |> Seq.toList), rest)
    
[<Fact>]
let ``NextSymbol: simple float non zero exponent with imaginary`` () =
    let symbol, text, rest = NextSymbol ("1.1e+4j" |> Seq.toList)
    Assert.Equivalent(Some(Token.Number), symbol)
    Assert.Equivalent(Some("1.1e+4j"), text)
    Assert.Equal<char list>(("" |> Seq.toList), rest)
    
[<Fact>]
let ``NextSymbol: Reserved keyword starts with type`` () =
    let symbol, text, rest = NextSymbol ("type" |> Seq.toList)
    Assert.Equivalent(Some(Token.Name), symbol) (* Soft keyword as Name *)
    Assert.Equivalent(Some("type"), text)
    Assert.Equal<char list>(("" |> Seq.toList), rest)
    
[<Fact>]
let ``NextSymbol: Reserved keyword starts with Type`` () =
    let symbol, text, rest = NextSymbol ("Type" |> Seq.toList)
    Assert.Equivalent(Some(Token.Name), symbol)
    Assert.Equivalent(Some("Type"), text)
    Assert.Equal<char list>(("" |> Seq.toList), rest)
    
[<Fact>]
let ``NextSymbol: Reserved keyword starts with try`` () =
    let symbol, text, rest = NextSymbol ("try" |> Seq.toList)
    Assert.Equivalent(Some(Token.Try), symbol)
    Assert.Equivalent(Option.None, text)
    Assert.Equal<char list>(("" |> Seq.toList), rest)
    
[<Fact>]
let ``NextSymbol: Reserved keyword starts with for`` () =
    let symbol, text, rest = NextSymbol ("for" |> Seq.toList)
    Assert.Equivalent(Some(Token.Name), symbol)
    Assert.Equivalent(Option.None, text)
    Assert.Equal<char list>(("" |> Seq.toList), rest)
    
[<Fact>]
let ``NextSymbol: Reserved keyword starts with First`` () =
    let symbol, text, rest = NextSymbol ("First" |> Seq.toList)
    Assert.Equivalent(Some(Token.Name), symbol)
    Assert.Equivalent(Some("First"), text)
    Assert.Equal<char list>(("" |> Seq.toList), rest)
    
[<Fact>]
let ``NextSymbol: Empty single quote string`` () =
    let symbol, text, rest = NextSymbol ("''" |> Seq.toList)
    Assert.Equivalent(Some(Token.String), symbol)
    Assert.Equivalent(Some("''"), text)
    Assert.Equal<char list>(("" |> Seq.toList), rest)
    
[<Fact>]
let ``NextSymbol: Empty double quote string`` () =
    let symbol, text, rest = NextSymbol ("\"\"" |> Seq.toList)
    Assert.Equivalent(Some(Token.String), symbol)
    Assert.Equivalent(Some("\"\""), text)
    Assert.Equal<char list>(("" |> Seq.toList), rest)
    
[<Fact>]
let ``NextSymbol: Empty triple single quote string`` () =
    let symbol, text, rest = NextSymbol ("''''''" |> Seq.toList)
    Assert.Equivalent(Some(Token.String), symbol)
    Assert.Equivalent(Some("''''''"), text)
    Assert.Equal<char list>(("" |> Seq.toList), rest)
    
[<Fact>]
let ``NextSymbol: Empty triple multiple quote string`` () =
    let symbol, text, rest = NextSymbol ("\"\"\"\"\"\"" |> Seq.toList)
    Assert.Equivalent(Some(Token.String), symbol)
    Assert.Equivalent(Some("\"\"\"\"\"\""), text)
    Assert.Equal<char list>(("" |> Seq.toList), rest)
    
[<Fact>]
let ``NextSymbol: single quote string`` () =
    let symbol, text, rest = NextSymbol ("'Hello, World!'" |> Seq.toList)
    Assert.Equivalent(Some(Token.String), symbol)
    Assert.Equivalent(Some("'Hello, World!'"), text)
    Assert.Equal<char list>(("" |> Seq.toList), rest)
    
[<Fact>]
let ``NextSymbol: triple single quote string`` () =
    let symbol, text, rest = NextSymbol ("'''Hello, World!'''" |> Seq.toList)
    Assert.Equivalent(Some(Token.String), symbol)
    Assert.Equivalent(Some("'''Hello, World!'''"), text)
    Assert.Equal<char list>(("" |> Seq.toList), rest)
    
[<Fact>]
let ``NextSymbol: double quote string`` () =
    let symbol, text, rest = NextSymbol ("\"Hello, World!\"" |> Seq.toList)
    Assert.Equivalent(Some(Token.String), symbol)
    Assert.Equivalent(Some("\"Hello, World!\""), text)
    Assert.Equal<char list>(("" |> Seq.toList), rest)
    
[<Fact>]
let ``NextSymbol: triple double quote string`` () =
    let symbol, text, rest = NextSymbol ("\"\"\"Hello, World!\"\"\"" |> Seq.toList)
    Assert.Equivalent(Some(Token.String), symbol)
    Assert.Equivalent(Some("\"\"\"Hello, World!\"\"\""), text)
    Assert.Equal<char list>(("" |> Seq.toList), rest)
    
[<Fact>]
let ``NextSymbol: prefix u for single quote`` () =
    let symbol, text, rest = NextSymbol ("u''" |> Seq.toList)
    Assert.Equivalent(Some(Token.String), symbol)
    Assert.Equivalent(Some("u''"), text)
    Assert.Equal<char list>(("" |> Seq.toList), rest)
    
[<Fact>]
let ``NextSymbol: prefix u for double quote`` () =
    let symbol, text, rest = NextSymbol ("u\"\"" |> Seq.toList)
    Assert.Equivalent(Some(Token.String), symbol)
    Assert.Equivalent(Some("u\"\""), text)
    Assert.Equal<char list>(("" |> Seq.toList), rest)
    
[<Fact>]
let ``NextSymbol: prefix r for single quote`` () =
    let symbol, text, rest = NextSymbol ("r''" |> Seq.toList)
    Assert.Equivalent(Some(Token.String), symbol)
    Assert.Equivalent(Some("r''"), text)
    Assert.Equal<char list>(("" |> Seq.toList), rest)
    
[<Fact>]
let ``NextSymbol: prefix r for double quote`` () =
    let symbol, text, rest = NextSymbol ("R\"\"" |> Seq.toList)
    Assert.Equivalent(Some(Token.String), symbol)
    Assert.Equivalent(Some("R\"\""), text)
    Assert.Equal<char list>(("" |> Seq.toList), rest)
    
[<Fact>]
let ``NextSymbol: prefix f for single quote`` () =
    let symbol, text, rest = NextSymbol ("f''" |> Seq.toList)
    Assert.Equivalent(Some(Token.String), symbol)
    Assert.Equivalent(Some("f''"), text)
    Assert.Equal<char list>(("" |> Seq.toList), rest)
    
[<Fact>]
let ``NextSymbol: prefix f for double quote`` () =
    let symbol, text, rest = NextSymbol ("f\"\"" |> Seq.toList)
    Assert.Equivalent(Some(Token.String), symbol)
    Assert.Equivalent(Some("f\"\""), text)
    Assert.Equal<char list>(("" |> Seq.toList), rest)

    
[<Fact>]
let ``NextSymbol: prefix t for single quote`` () =
    let symbol, text, rest = NextSymbol ("t''" |> Seq.toList)
    Assert.Equivalent(Some(Token.String), symbol)
    Assert.Equivalent(Some("t''"), text)
    Assert.Equal<char list>(("" |> Seq.toList), rest)
    
[<Fact>]
let ``NextSymbol: prefix t for double quote`` () =
    let symbol, text, rest = NextSymbol ("t\"\"" |> Seq.toList)
    Assert.Equivalent(Some(Token.String), symbol)
    Assert.Equivalent(Some("t\"\""), text)
    Assert.Equal<char list>(("" |> Seq.toList), rest)
    
[<Fact>]
let ``NextSymbol: prefix t for triple single quote`` () =
    let symbol, text, rest = NextSymbol ("t''''''" |> Seq.toList)
    Assert.Equivalent(Some(Token.String), symbol)
    Assert.Equivalent(Some("t''''''"), text)
    Assert.Equal<char list>(("" |> Seq.toList), rest)
    
[<Fact>]
let ``NextSymbol: prefix t for triple double quote`` () =
    let symbol, text, rest = NextSymbol ("t\"\"\"\"\"\"" |> Seq.toList)
    Assert.Equivalent(Some(Token.String), symbol)
    Assert.Equivalent(Some("t\"\"\"\"\"\""), text)
    Assert.Equal<char list>(("" |> Seq.toList), rest)
    
[<Fact>]
let ``NextSymbol: prefix rt for single quote`` () =
    let symbol, text, rest = NextSymbol ("rt''" |> Seq.toList)
    Assert.Equivalent(Some(Token.String), symbol)
    Assert.Equivalent(Some("rt''"), text)
    Assert.Equal<char list>(("" |> Seq.toList), rest)
    
[<Fact>]
let ``NextSymbol: prefix rt for double quote`` () =
    let symbol, text, rest = NextSymbol ("Rt\"\"" |> Seq.toList)
    Assert.Equivalent(Some(Token.String), symbol)
    Assert.Equivalent(Some("Rt\"\""), text)
    Assert.Equal<char list>(("" |> Seq.toList), rest)
    
[<Fact>]
let ``NextSymbol: prefix rf for single quote`` () =
    let symbol, text, rest = NextSymbol ("rf''" |> Seq.toList)
    Assert.Equivalent(Some(Token.String), symbol)
    Assert.Equivalent(Some("rf''"), text)
    Assert.Equal<char list>(("" |> Seq.toList), rest)
    
[<Fact>]
let ``NextSymbol: prefix rf for double quote`` () =
    let symbol, text, rest = NextSymbol ("RF\"\"" |> Seq.toList)
    Assert.Equivalent(Some(Token.String), symbol)
    Assert.Equivalent(Some("RF\"\""), text)
    Assert.Equal<char list>(("" |> Seq.toList), rest)
    
[<Fact>]
let ``NextSymbol: tripple single quote with newline`` () =
    let symbol, text, rest = NextSymbol ("rt'''Test\r\nAgain'''" |> Seq.toList)
    Assert.Equivalent(Some(Token.String), symbol)
    Assert.Equivalent(Some("rt'''Test\r\nAgain'''"), text)
    Assert.Equal<char list>(("" |> Seq.toList), rest)
    
[<Fact>]
let ``NextSymbol: triple double quote with newline`` () =
    let symbol, text, rest = NextSymbol ("Rt\"\"\"Test\r\nAgain\"\"\"" |> Seq.toList)
    Assert.Equivalent(Some(Token.String), symbol)
    Assert.Equivalent(Some("Rt\"\"\"Test\r\nAgain\"\"\""), text)
    Assert.Equal<char list>(("" |> Seq.toList), rest)
    
[<Fact>]
let ``Name literal token`` () =
    let symbols = "__init__" |> Tokenize
    Assert.Equal(Ok([ Token.NameLiteral(0u, 8u, "__init__");  ]), symbols)
    
[<Fact>]
let ``Number literal token`` () =
    let symbols = "0.0j" |> Tokenize
    Assert.Equal(Ok([ Token.NumberLiteral(0u, 4u, "0.0j");  ]), symbols)
    
[<Fact>]
let ``String literal token`` () =
    let symbols = "r'Hello, World!'" |> Tokenize
    Assert.Equal(Ok([ Token.StringLiteral(0u, 16u, "r'Hello, World!'");  ]), symbols)
    
[<Fact>]
let ``power token`` () =
    let symbols = "**" |> Tokenize
    Assert.Equal(Ok([ Token.Power(0u, 2u);  ]), symbols)
    
[<Fact>]
let ``Parenthesis '('`` () =
    let symbols = "(" |> Tokenize
    Assert.Equal(Ok([ Token.LeftParen(0u, 1u);  ]), symbols)
    
[<Fact>]
let ``Parenthesis '(' and ')'`` () =
    let symbols = "()" |> Tokenize
    Assert.Equal(Ok([ Token.LeftParen(0u, 1u); Token.RightParen(1u, 2u);  ]), symbols)
    
[<Fact>]
let ``Parenthesis '['`` () =
    let symbols = "[" |> Tokenize
    Assert.Equal(Ok([ Token.LeftBracket(0u, 1u);  ]), symbols)
    
[<Fact>]
let ``Parenthesis '[' and ']'`` () =
    let symbols = "[]" |> Tokenize
    Assert.Equal(Ok([ Token.LeftBracket(0u, 1u); Token.RightBracket(1u, 2u);  ]), symbols)
    
[<Fact>]
let ``Parenthesis '{'`` () =
    let symbols = "{" |> Tokenize
    Assert.Equal(Ok([ Token.LeftCurly(0u, 1u);  ]), symbols)
    
[<Fact>]
let ``Parenthesis '}' and ']'`` () =
    let symbols = "{}" |> Tokenize
    Assert.Equal(Ok([ Token.LeftCurly(0u, 1u); Token.RightCurly(1u, 2u);  ]), symbols)
    
[<Fact>]
let ``Tokens and whitespace`` () =
    let symbols = "1 + 2" |> Tokenize
    Assert.Equal(Ok([ Token.NumberLiteral(0u, 1u, "1"); Token.Plus(2u, 3u); Token.NumberLiteral(4u, 5u, "2");  ]), symbols)
    
[<Fact>]
let ``Simple comment line`` () =
    let symbols = "# This is a comment" |> Tokenize
    Assert.Equal(Ok([ ]), symbols) 