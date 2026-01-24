module SimplyPythonNet.parser

open SimplyPythonNet.tokenizer

type AST =
    |   Empty
    |   True of uint * uint
    |   False of uint * uint
    |   None of uint * uint
    |   Ellipsis of uint * uint
    |   Number of uint * uint * string
    |   Name of uint * uint * string
    |   String of uint * uint * string list
    |   Await of uint * uint * AST
    |   Power of uint * uint * AST * AST
    |   UnaryPlus of uint * uint * AST
    |   UnaryMinus of uint * uint * AST
    |   Invert of uint * uint * AST
    |   Mul of uint * uint * AST * AST
    |   Slash of uint * uint * AST * AST
    |   DoubleSlash of uint * uint * AST * AST
    |   Modulo of uint * uint * AST * AST
    |   At of uint * uint * AST * AST
    |   Plus of uint * uint * AST * AST
    |   Minus of uint * uint * AST * AST
    
type NodeThree = Result<AST * TokenStream, string * uint>

(* Utilities functions*)
let GetStartOfTokenInStream (tokens: TokenStream) : uint =
    match tokens with
    |   [] -> 0u
    |   first :: _ -> GetTokenStartPosition first
    
(* Expression rules *)
let rec ParseAtom (stream: Result<TokenStream, string * uint> ) : NodeThree =
    match stream with
    |   Error(e, p) -> Error(e, p)
    |   Ok(stream_ok) ->
        match stream_ok with
        | [] -> Error ("Unexpected end of input", 0u)
        | first :: rest ->
            match first with
            |   Token.True(s, e) -> Ok(AST.True(s, e), rest)
            |   Token.False(s, e) -> Ok(AST.False(s, e), rest)
            |   Token.None(s, e) -> Ok(AST.None(s, e), rest)
            |   Token.Ellipsis(s, e) -> Ok(AST.Ellipsis(s, e), rest)
            |   Token.NameLiteral(s, e, t) -> Ok(AST.Name(s, e, t), rest)
            |   Token.NumberLiteral(s, e, t) -> Ok(AST.Number(s, e, t), rest)
            |   Token.StringLiteral(s, e, t) ->
                    let mutable text = [ t ]
                    let start = s
                    let mutable _end = e
                    let mutable rest_symbols = rest
                    while
                        match rest_symbols with
                        | [] -> false
                        | first :: rest2 ->
                            match first with
                            |   Token.StringLiteral(_, e, t) ->
                                    text <- t :: text
                                    _end <- e
                                    rest_symbols <- rest2
                                    true
                            |   _ -> false
                        do ()
                    Ok(AST.String(start, _end, List.rev text), rest_symbols)
            |   _  -> Error ("Unexpected token", GetTokenStartPosition first)
            
and ParsePrimary (stream: Result<TokenStream, string * uint> ) : NodeThree =
    match stream with
    |   Error(e, p) -> Error(e, p)
    |   Ok _ ->
            let left = ParseAtom stream
            match left with
            |   Error(e, p) -> Error(e, p)
            |   Ok(stream_ok) ->
                    let left2, rest = stream_ok
                    match rest with
                    | [] -> Ok(left2, rest) (* End of input, but valid expression *)
                    | first :: _ ->
                        match first with
                        | Token.LeftParen _  | Token.LeftBracket _ | Token.Period _  -> Error ("TODO! In Parser", GetTokenStartPosition first)
                        |   _  -> Ok((left2, rest))

and ParseAwaitPrimary (stream: Result<TokenStream, string * uint> ) : NodeThree =
    match stream with
    |   Error(e, p) -> Error(e, p)
    |   Ok(stream_ok) ->
        match stream_ok with
        | [] -> Error ("Unexpected end of input", 0u)
        | first :: rest ->
            match first with
            |   Token.Await(s, _) ->
                    let right = ParsePrimary (Ok(rest))
                    match right with
                    |   Ok(right_ok) ->
                            let right3, rest2 = right_ok
                            Ok(AST.Await(s, GetStartOfTokenInStream rest2, right3), rest2)
                    |   Error(e, p) -> Error(e, p)
                    
            |   _  -> ParsePrimary stream
            
and ParsePower(stream: Result<TokenStream, string * uint> ) : NodeThree =    
    match stream with
    |   Error(e, p) -> Error(e, p)
    |   Ok check ->
            let s = GetStartOfTokenInStream check
            let left = ParseAwaitPrimary stream
            match left with
            |   Error(e, p) -> Error(e, p)
            |   Ok(stream_ok) ->
                    let left2, rest = stream_ok
                    match rest with
                    | [] -> Ok(left2, rest) (* End of input, but valid expression *)
                    | first :: rest2 ->
                        match first with
                        | Token.Power _  ->
                            let right = ParseFactor (Ok(rest2))
                            match right with
                            |   Error(e, p) -> Error(e, p)
                            |   Ok(stream_ok_2) ->
                                    let right2, rest3 = stream_ok_2
                                    Ok(AST.Power(s, GetStartOfTokenInStream rest3, left2, right2), rest3)  
                        |   _  -> Ok((left2, rest))
                        
and ParseFactor(stream: Result<TokenStream, string * uint> ) : NodeThree =    
    match stream with
    |   Error(e, p) -> Error(e, p)
    |   Ok stream_ok ->
            match stream_ok with
                    | [] -> Error("Unexpected end of input", 0u)
                    | first :: rest2 ->
                        match first with
                        | Token.Plus (s, _)  ->
                            let right = ParseFactor (Ok(rest2))
                            match right with
                            |   Error(e, p) -> Error(e, p)
                            |   Ok(stream_ok_2) ->
                                    let right2, rest3 = stream_ok_2
                                    Ok(AST.UnaryPlus(s, GetStartOfTokenInStream rest3, right2), rest3)
                        | Token.Minus (s, _)  ->
                            let right = ParseFactor (Ok(rest2))
                            match right with
                            |   Error(e, p) -> Error(e, p)
                            |   Ok(stream_ok_2) ->
                                    let right2, rest3 = stream_ok_2
                                    Ok(AST.UnaryMinus(s, GetStartOfTokenInStream rest3, right2), rest3)
                        | Token.BitwiseInvert (s, _)  ->
                            let right = ParseFactor (Ok(rest2))
                            match right with
                            |   Error(e, p) -> Error(e, p)
                            |   Ok(stream_ok_2) ->
                                    let right2, rest3 = stream_ok_2
                                    Ok(AST.Invert(s, GetStartOfTokenInStream rest3, right2), rest3)  
                        |   _  -> ParsePower stream

and ParseTerm(stream: Result<TokenStream, string * uint> ) : NodeThree =
    match stream with
    |   Error(e, p) -> Error(e, p)
    |   Ok check ->
            let s = GetStartOfTokenInStream check
            let left = ParseFactor stream
            match left with
            |   Error(e, p) -> Error(e, p)
            |   Ok(stream_ok) ->
                    let left2, rest = stream_ok
                    match rest with
                    | [] -> Ok(left2, rest) (* End of input, but valid expression *)
                    | first :: rest2 ->
                        match first with
                        | Token.Mul _ | Token.Slash _ | Token.DoubleSlash _ | Token.Modulo _ | Token.Matrices _ ->
                            let right = ParseFactor (Ok(rest2))
                            match right with
                            |   Error(e, p) -> Error(e, p)
                            |   Ok(stream_ok_2) ->
                                    let right2, rest3 = stream_ok_2
                                    Ok(AST.Power(s, GetStartOfTokenInStream rest3, left2, right2), rest3)  
                        |   _  -> left
