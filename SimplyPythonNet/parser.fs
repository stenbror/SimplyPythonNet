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
    
type NodeThree = Result<(AST * TokenStream), (string * uint)>

(* Utilities functions*)
let GetStartOfTokenInStream (tokens: TokenStream) : uint =
    match tokens with
    |   [] -> 0u
    |   first :: rest -> GetTokenStartPosition first
    
(* Expression rules *)
let rec ParseAtom (stream: Result<TokenStream, (string * uint)> ) : NodeThree =
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
                            |   Token.StringLiteral(s, e, t) ->
                                    text <- t :: text
                                    _end <- e
                                    rest_symbols <- rest2
                                    true
                            |   _ -> false
                        do ()
                    Ok(AST.String(start, _end, List.rev text), rest_symbols)
            |   _  -> Error ("Unexpected token", GetTokenStartPosition first)
            
and ParsePrimary (stream: Result<TokenStream, (string * uint)> ) : NodeThree =
    match stream with
    |   Error(e, p) -> Error(e, p)
    |   Ok(stream_ok) ->
        match stream_ok with
        | [] -> Error ("Unexpected end of input", 0u)
        | first :: rest ->
            match first with
            |   _  -> Error ("Unexpected token", GetTokenStartPosition first)

and ParseAwaitPrimary (stream: Result<TokenStream, (string * uint)> ) : NodeThree =
    match stream with
    |   Error(e, p) -> Error(e, p)
    |   Ok(stream_ok) ->
        match stream_ok with
        | [] -> Error ("Unexpected end of input", 0u)
        | first :: rest ->
            match first with
            |   Token.Await(s, e) ->
                    let right = ParsePrimary (Ok(rest))
                    match right with
                    |   Ok(right_ok) ->
                            let right3, rest2 = right_ok
                            Ok(AST.Await(s, GetStartOfTokenInStream rest2, right3), rest2)
                    |   Error(e, p) -> Error(e, p)
                    
            |   _  -> Error ("Unexpected token", GetTokenStartPosition first)
            
            
            
            
            

// Empty template function to be used for rules ///////////////////////////////////////////////////////////////////////        
let DummyFunction (stream: Result<TokenStream, (string * uint)> ) : NodeThree =
    match stream with
    |   Error(e, p) -> Error(e, p)
    |   Ok(stream_ok) ->
        match stream_ok with
        | [] -> Error ("Unexpected end of input", 0u)
        | first :: rest ->
            match first with
            |   _  -> Error ("Unexpected token", GetTokenStartPosition first)