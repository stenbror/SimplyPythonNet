module SimplyPythonNet.tokenizer

open System

type Token =
    |   Empty
    (* Reserved keywords *)
    |   False of uint * uint
    |   None of uint * uint
    |   True of uint * uint
    |   And of uint * uint
    |   As of uint * uint
    |   Assert of uint * uint
    |   Async of uint * uint
    |   Await of uint * uint
    |   Break of uint * uint
    |   Class of uint * uint
    |   Continue of uint * uint
    |   Def of uint * uint
    |   Del of uint * uint
    |   Elif of uint * uint
    |   Else of uint * uint
    |   Except of uint * uint
    |   Finally of uint * uint
    |   For of uint * uint
    |   From of uint * uint
    |   Global of uint * uint
    |   If of uint * uint
    |   In of uint * uint
    |   Is of uint * uint
    |   Lambda of uint * uint
    |   Nonlocal of uint * uint
    |   Not of uint * uint
    |   Or of uint * uint
    |   Pass of uint * uint
    |   Raise of uint * uint
    |   Return of uint * uint
    |   Try of uint * uint
    |   While of uint * uint
    |   With of uint * uint
    |   Yield of uint * uint
    (* Soft keywords *)
    |   Match of uint * uint
    |   Case of uint * uint
    |   Type of uint * uint
    |   Default of uint * uint (* '_' *)
    (* Operators *)
    |   PlusAssign of uint * uint
    |   MinusAssign of uint * uint
    |   MulAssign of uint * uint
    |   PowerAssign of uint * uint
    |   SlashAssign of uint * uint
    |   DoubleSlashAssign of uint * uint
    |   ModuloAssign of uint * uint
    |   BitwiseAndAssign of uint * uint
    |   BitwiseOrAssign of uint * uint
    |   BitwiseXorAssign of uint * uint
    |   BitwiseShiftLeftAssign of uint * uint
    |   BitwiseShiftRightAssign of uint * uint
    |   MatricesAssign of uint * uint
    |   ColonAssign of uint * uint
    |   BitwiseAnd of uint * uint
    |   BitwiseOr of uint * uint
    |   BitwiseXor of uint * uint
    |   BitwiseInvert of uint * uint
    |   BitwiseShiftLeft of uint * uint
    |   BitwiseShiftRight of uint * uint
    |   LessEqual of uint * uint
    |   GreaterEqual of uint * uint
    |   Less of uint * uint
    |   Greater of uint * uint
    |   Equal of uint * uint
    |   NotEqual of uint * uint
    |   LeftParen of uint * uint
    |   RightParen of uint * uint
    |   LeftBracket of uint * uint
    |   RightBracket of uint * uint
    |   LeftCurly of uint * uint
    |   RightCurly of uint * uint
    |   Comma of uint * uint
    |   Colon of uint * uint
    |   BitwiseNot of uint * uint
    |   SemiColon of uint * uint
    |   Assign of uint * uint
    |   Arrow of uint * uint
    |   Plus of uint * uint
    |   Minus of uint * uint
    |   Power of uint * uint
    |   Mul of uint * uint
    |   DoubleSlash of uint * uint
    |   Slash of uint * uint
    |   Modulo of uint * uint
    |   Period of uint * uint
    |   Matrices of uint * uint
    |   Ellipsis of uint * uint
    (* Literals *)
    |   Name of uint * uint
    |   NameLiteral of uint * uint * string
    |   Number of uint * uint
    |   NumberLiteral of uint * uint * string
    |   String of uint * uint
    |   StringLiteral of uint * uint * string
    
let OneCharToken c =
    match c with
    |   '&' -> Some(Token.BitwiseAnd)
    |   '|' -> Some(Token.BitwiseOr)
    |   '^' -> Some(Token.BitwiseXor)
    |   '~' -> Some(Token.BitwiseInvert)
    |   '<' -> Some(Token.Less)
    |   '>' -> Some(Token.Greater)
    |   '(' -> Some(Token.LeftParen)
    |   ')' -> Some(Token.RightParen)
    |   '[' -> Some(Token.LeftBracket)
    |   ']' -> Some(Token.RightParen)
    |   '{' -> Some(Token.LeftCurly)
    |   '}' -> Some(Token.RightCurly)
    |   ',' -> Some(Token.Comma)
    |   ':' -> Some(Token.Colon)
    |   '!' -> Some(Token.BitwiseNot)
    |   ';' -> Some(Token.SemiColon)
    |   '=' -> Some(Token.Assign)
    |   '+' -> Some(Token.Plus)
    |   '-' -> Some(Token.Minus)
    |   '*' -> Some(Token.Mul)
    |   '/' -> Some(Token.Slash)
    |   '%' -> Some(Token.Modulo)
    |   '@' -> Some(Token.Matrices)
    |   '.' -> Some(Token.Period)
    |   _ -> Option.None
        
let TwoCharToken (c1 : char, c2 : char) =
     match c1, c2 with
     | '+', '=' -> Some(Token.PlusAssign)
     | '-', '=' -> Some(Token.MinusAssign)
     | '*', '=' -> Some(Token.MulAssign)
     | '/', '=' -> Some(Token.SlashAssign)
     | '%', '=' -> Some(Token.ModuloAssign)
     | '&', '=' -> Some(Token.BitwiseAndAssign)
     | '|', '=' -> Some(Token.BitwiseOrAssign)
     | '^', '=' -> Some(Token.BitwiseXorAssign)
     | '@', '=' -> Some(Token.MatricesAssign)
     | ':', '=' -> Some(Token.ColonAssign)
     | '<', '<' -> Some(Token.BitwiseShiftLeft)
     | '>', '>' -> Some(Token.BitwiseShiftRight)
     | '<', '=' -> Some(Token.LessEqual)
     | '>', '=' -> Some(Token.GreaterEqual)
     | '=', '=' -> Some(Token.Equal)
     | '!', '=' -> Some(Token.NotEqual)
     | '-', '>' -> Some(Token.Arrow)
     | '/', '/' -> Some(Token.DoubleSlash)
     | '*', '*' -> Some(Token.Power)
     |   _ -> Option.None
     
let ThreeCharToken (c1: char, c2: char, c3: char)=
     match c1, c2, c3 with
     | '<', '<', '=' -> Some(Token.BitwiseShiftLeftAssign)
     | '>', '>', '=' -> Some(Token.BitwiseShiftRightAssign)
     | '*', '*', '=' -> Some(Token.PowerAssign)
     | '/', '/', '=' -> Some(Token.DoubleSlashAssign)
     | '.', '.', '.' -> Some(Token.Ellipsis)
     |  _ -> Option.None
     
let ReservedKeyword word =
    match word with
    |   "False"     -> Some(Token.False)
    |   "None"      -> Some(Token.None)
    |   "True"      -> Some(Token.True)
    |   "and"       -> Some(Token.And)
    |   "as"        -> Some(Token.As)
    |   "assert"    -> Some(Token.Assert)
    |   "async"     -> Some(Token.Async)
    |   "await"     -> Some(Token.Await)
    |   "break"     -> Some(Token.Break)
    |   "class"     -> Some(Token.Class)
    |   "continue"  -> Some(Token.Continue)
    |   "def"       -> Some(Token.Def)
    |   "del"       -> Some(Token.Del)
    |   "elif"      -> Some(Token.Elif)
    |   "else"      -> Some(Token.Else)
    |   "except"    -> Some(Token.Except)
    |   "finally"   -> Some(Token.Finally)
    |   "for"       -> Some(Token.For)
    |   "from"      -> Some(Token.From)
    |   "global"    -> Some(Token.Global)
    |   "if"        -> Some(Token.If)
    |   "in"        -> Some(Token.In)
    |   "is"        -> Some(Token.Is)
    |   "lambda"    -> Some(Token.Lambda)
    |   "nonlocal"  -> Some(Token.Nonlocal)
    |   "not"       -> Some(Token.Not)
    |   "or"        -> Some(Token.Or)
    |   "pass"      -> Some(Token.Pass)
    |   "raise"     -> Some(Token.Raise)
    |   "return"    -> Some(Token.Return)
    |   "try"       -> Some(Token.Try)
    |   "while"     -> Some(Token.While)
    |   "with"      -> Some(Token.With)
    |   "yield"     -> Some(Token.Yield)
    |   _           -> Option.None
    
let SoftKeyword (symbol : Token) : Option<Token> =
    match symbol with
    |   Token.NameLiteral(s, e, t) ->
            match t with
            |   "match" -> Some(Token.Match(s, e))
            |   "case"  -> Some(Token.Case(s, e))
            |   "type"  -> Some(Token.Type(s, e))
            |   "_"     -> Some(Token.Default(s, e))
            |   _       -> Option.None
    |   _ -> Option.None
    
let PeekNextThreeChars (chars : char list) : (char * char * char) =
    let one, rest1 = match chars with
                     | head :: rest -> head, rest
                     | [] -> '\u0000', chars
    let two, rest2 = match rest1 with
                     | head :: rest -> head, rest
                     | [] -> '\u0000', rest1
    let three, _ =   match rest2 with
                     | head :: rest -> head, rest
                     | [] -> '\u0000', rest2
                                
    (one, two, three)

let PeekNextTwoChars (chars : char list) : (char * char) =
    let one, rest1 = match chars with
                     | head :: rest -> head, rest
                     | [] -> '\u0000', chars
    let two, _ = match rest1 with
                 | head :: rest -> head, rest
                 | [] -> '\u0000', rest1
                              
    (one, two)
    
let PeekNextChar (chars : char list) : char =
    let one, _ = match chars with
                 | head :: rest -> head, rest
                 | [] -> '\u0000', chars
    
    one
    
let AdvanceCharacters (chars : char list, steps: uint) : char list =
    let mutable res : char list = chars
    for _ in 1u .. steps do
        res <- match res with
               | _ :: tail -> tail
               | [] -> res
               
    res
    
let IsHexDigit (c : char) : bool =
    match c with
    | '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' 
    | 'a' | 'b' | 'c' | 'd' | 'e' | 'f' | 'A' | 'B' | 'C' | 'D' | 'E' | 'F' -> true
    | _ -> false
    
let IsOctetDigit (c : char) : bool =
    match c with
    | '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' -> true
    | _ -> false
    
let IsBinaryDigit (c : char) : bool =
    match c with
    | '0' | '1' -> true
    | _ -> false
    
let Operators(chars: char list) : (uint * uint -> Token) option * char list =
    match ThreeCharToken(PeekNextThreeChars(chars)) with
    | Some(token) ->
        (Some(token), AdvanceCharacters(chars, 3u))
    | Option.None ->
        match TwoCharToken(PeekNextTwoChars(chars)) with
        | Some(token) ->
            (Some(token), AdvanceCharacters(chars, 2u))
        | Option.None ->
            let res = OneCharToken(PeekNextChar(chars))
            match res with
            | Some(token) ->
                (Some(token), AdvanceCharacters(chars, 1u))
            | Option.None ->
                (Option.None, chars)
                
let ReadHexadecimalNumber (chars : char list) : bool * string * char list =
    let mutable text = String.Empty
    let mutable res = chars
    let mutable ok = true
    
    while
        match PeekNextChar res with
        | '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' 
        | 'a' | 'b' | 'c' | 'd' | 'e' | 'f' | 'A' | 'B' | 'C' | 'D' | 'E' | 'F' ->
            text <- text + string(PeekNextChar res)
            res <- AdvanceCharacters (res, 1u)
            true
        | '_' ->
            res <- AdvanceCharacters (res, 1u)
            if IsHexDigit(PeekNextChar res) = false then
                ok <- false
                text <- "Invalid digit after '_' in hexadecimal number"
                false
            else true
        | _ -> false
        do ()
    
    (ok, text, res)
    
let ReadOctalNumber (chars : char list) : bool * string * char list =
    let mutable text = String.Empty
    let mutable res = chars
    let mutable ok = true
    
    while
        match PeekNextChar res with
        | '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' ->
            text <- text + string(PeekNextChar res)
            res <- AdvanceCharacters (res, 1u)
            true
        | '_' ->
            res <- AdvanceCharacters (res, 1u)
            if IsOctetDigit(PeekNextChar res) = false then
                ok <- false
                text <- "Invalid digit after '_' in octal number"
                false
            else true
        | _ -> false
        do ()
    
    (ok, text, res)
    
let ReadBinaryNumber (chars : char list) : bool * string * char list =
    let mutable text = String.Empty
    let mutable res = chars
    let mutable ok = true
    
    while
        match PeekNextChar res with
        | '0' ->
            if text <> String.Empty then
                text <- text + string(PeekNextChar res)
                res <- AdvanceCharacters (res, 1u)
                true
            else (* Ignore leading zeros in binary number *)
                res <- AdvanceCharacters (res, 1u)
                false
        | '1' ->
            text <- text + string(PeekNextChar res)
            res <- AdvanceCharacters (res, 1u)
            true
        | '_' ->
            res <- AdvanceCharacters (res, 1u)
            if IsBinaryDigit(PeekNextChar res) = false then
                ok <- false
                text <- "Invalid digit after '_' in binary number"
                false
            else true
        | _ -> false
        do ()
    
    (ok, text, res)
                
let ReadExponent (chars : char list) : bool * string * char list =
    let mutable text = string(PeekNextChar chars)
    let mutable res = AdvanceCharacters(chars, 1u)
    let mutable ok = true
    
    match PeekNextChar res with
    | '+' | '-' ->
        text <- text + string(PeekNextChar res)
        res <- AdvanceCharacters(res, 1u)
    | _ -> ()
    
    while
          match PeekNextChar res with
          | '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' ->
                text <- text + string(PeekNextChar res)
                res <- AdvanceCharacters (res, 1u)
                if PeekNextChar res = '_' then
                    res <- AdvanceCharacters (res, 1u)
                    if Char.IsDigit(PeekNextChar res) = false then
                        ok <- false
                        text <- "Invalid digit after '_' in fraction"
                ok
          | 'j' | 'J' ->
                text <- text + string(PeekNextChar res)
                res <- AdvanceCharacters (res, 1u)
                false
          | _ -> false
          do ()
    
    (ok, text, res)
                
let ReadFraction (chars : char list) : bool * string * char list =
    let mutable res = AdvanceCharacters(chars, 1u)
    let mutable text = "."
    let mutable ok = true
    
    if Char.IsDigit(PeekNextChar res) = false then
        (false, "Expecting digit after '.' in fraction", res)
    else
        while 
            match PeekNextChar res with
            | '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' ->
                text <- text + string(PeekNextChar res)
                res <- AdvanceCharacters (res, 1u)
                if PeekNextChar res = '_' then
                    res <- AdvanceCharacters (res, 1u)
                    if Char.IsDigit(PeekNextChar res) = false then
                        ok <- false
                        text <- "Invalid digit after '_' in fraction"
                ok
            | 'e' | 'E' ->
                let (ok2, text2, rest) = ReadExponent res
                match ok2 with
                | true -> text <- text + text2
                | false -> text <- text2
                res <- rest
                ok <- ok2
                false
            | 'j' | 'J' ->
                text <- text + string(PeekNextChar res)
                res <- AdvanceCharacters (res, 1u)
                false
            | _ -> false
            do ()
        
        (ok, text, res)
        
let ReadNumber (chars : char list) : (uint * uint -> Token) option * string option * char list =
    let mutable res = chars
    let mutable text = String.Empty
    let mutable produced : (uint * uint -> Token) option * string option * char list = Option.None, Option.None, res
    
    if PeekNextChar (res) = '0' then
        res <- AdvanceCharacters (res, 1u)
        match PeekNextChar res with
        | 'x' | 'X' -> (* Handle Hexadecimal Numbers *)
            res <- AdvanceCharacters (res, 1u)
            let (ok, text2, rest) = ReadHexadecimalNumber res
            match ok with
            | true ->
                text <- "0x" + text2
                produced <- (Some(Token.Number), Some(text), rest)
            | false ->
                text <- text2
                produced <- (Option.None, Some(text), res) (* Invalid hexadecimal number detected *)
        | 'b' | 'B' -> (* Handle Binary Numbers *)
            res <- AdvanceCharacters (res, 1u)
            let (ok, text2, rest) = ReadBinaryNumber res
            match ok with
            | true ->
                text <- "0b" + text2
                produced <- (Some(Token.Number), Some(text), rest)
            | false ->
                text <- text2
                produced <- (Option.None, Some(text), res) (* Invalid binary number detected *)
        | 'o' | 'O' -> (* Handle Octal Numbers *)
            res <- AdvanceCharacters (res, 1u)
            let (ok, text2, rest) = ReadOctalNumber res
            match ok with
            | true ->
                text <- "0o" + text2
                produced <- (Some(Token.Number), Some(text), rest)
            | false ->
                text <- text2
                produced <- (Option.None, Some(text), res) (* Invalid octal number detected *)
        | _ ->
            while (* Ignore leading zeros in decimal number *)
                match PeekNextChar res with
                | '_' ->
                    res <- AdvanceCharacters (res, 1u)
                    if PeekNextChar res <> '0' then
                        text <- "Expecting digit after '_' in number"
                        produced <- (Option.None, Some(text), res)
                        false
                    else true
                | '0' ->
                    res <- AdvanceCharacters (res, 1u)
                    true
                | _ -> false
                do ()
            text <- "0"
    else
        text <- string(PeekNextChar res)
        res <- AdvanceCharacters (res, 1u)
        
    match produced with
    | (Option.None, Some(text), _) -> produced (* Invalid number detected that starts with zero *)
    | (Option.None, Option.None, _) ->
        let mutable ok = true
        while
            match PeekNextChar res with
            | '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' ->
                text <- text + string(PeekNextChar res)
                res <- AdvanceCharacters (res, 1u)
                true
            | 'j' | 'J' ->
                text <- text + string(PeekNextChar res)
                res <- AdvanceCharacters (res, 1u)
                false
            | '.' ->
                let (ok2, text2, rest) = ReadFraction res
                match ok2 with
                | true -> text <- text + text2
                | false -> text <- text2
                res <- rest
                ok <- ok2
                false
            | 'e' | 'E' ->
                let (ok2, text2, rest) = ReadExponent res
                match ok2 with
                | true -> text <- text + text2
                | false -> text <- text2
                res <- rest
                ok <- ok2
                false
            | '_' ->
                res <- AdvanceCharacters (res, 1u)
                if Char.IsDigit(PeekNextChar res) = false then
                    ok <- false
                    text <- "Invalid digit after '_' in decimal number"
                    false
                else true
            | _ -> false
            do ()
        
        match ok with
            | true -> (Some(Token.Number), Some(text), res)
            | false -> (Option.None, Some(text), res) (* Invalid fraction number detected *)
    | _ ->  produced (* Valid number detected starting with zero *)
        
let NextSymbol (chars : char list) : (uint * uint -> Token) option * string option * char list =
    let mutable res = chars
    match PeekNextChar res with
    | '\u0000' -> (Option.None, Option.None, res)
    | '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' -> (* Handle all Numbers *)
        ReadNumber(res)
    | 'u' | 'U' ->
        (Option.None, Option.None, res)
    | 'r' | 'R' ->
        (Option.None, Option.None, res)
    | 'f' | 'F' ->
        (Option.None, Option.None, res)
    | 'b' | 'B' ->
        (Option.None, Option.None, res)
    | '.' ->
        match PeekNextThreeChars res with
        | '.', '.', '.' -> (* Handle Ellipsis *)
            (Some(Token.Ellipsis), Option.None, AdvanceCharacters(res, 3u))
        | '.', digit, _ when digit >= '0' && digit <= '9' -> (* Handle Fraction Number *)
            let (ok, text, rest) = ReadFraction res
            match ok with
            | true -> (Some(Token.Number), Some(text), rest)
            | false -> (Option.None, Some(text), rest) (* Invalid fraction number detected *)
        | _ ->
            (Some(Token.Period), Option.None, AdvanceCharacters(res, 1u))
    | ''' | '"' ->
        (Option.None, Option.None, res)
    | _ ->
        match Operators(res) with (* Check for all valid Operators *)
        | Some(token), rest2 ->
            (Some(token), Option.None, rest2)
        | Option.None, _ ->
            if Char.IsLetter(PeekNextChar res) || PeekNextChar(res) = '_' then (* Handle all Names or Reserved Keywords *)
                let mutable text = ""
                while Char.IsLetterOrDigit(PeekNextChar res) || PeekNextChar(res) = '_' do
                    text <- text + string(PeekNextChar res)
                    res <- AdvanceCharacters(res, 1u)
                match ReservedKeyword(text) with
                | Some(token) ->
                    (Some(token), Option.None, res)
                | Option.None -> 
                    (Some(Token.Name), Some(text), res)
            else (Option.None, Option.None, res)
    
let Tokenize (code : string) : Token list =
    let mutable chars = code |> Seq.toList
    let mutable tokens : Token list = []
    
    List.rev tokens