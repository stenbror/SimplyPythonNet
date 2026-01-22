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
    
type NodeThree = Result<(AST * TokenStream), (string * uint)>
    
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
            |   _ -> Error ("Unexpected token", 0u)
