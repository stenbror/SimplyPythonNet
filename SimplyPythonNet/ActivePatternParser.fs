module SimplyPythonNet.ActivePatternParser

type Symbol =
    | Name of uint * uint * string
    | Number of uint * uint * string
    | String of uint * uint * string list
    | Ellipsis of uint * uint
    | None of uint * uint
    | False of uint * uint
    | True of uint * uint
    | Await of uint * uint
    | Plus of uint * uint
    
type AST =
    | Empty
    | Name of uint * uint * string
    | Number of uint * uint * string
    | String of uint * uint * string list
    | Ellipsis of uint * uint
    | None of uint * uint
    | False of uint * uint
    | True of uint * uint
    | Await of uint * uint * AST
    | Plus of AST * Symbol * AST
    
type SymbolStream = Symbol list
type NodeTree = AST * SymbolStream

(* Utilities functions *)

let GetStartPosition (stream : SymbolStream) : uint =
    match stream with
    | _ -> 0u
    
let GetEndPosition (stream : SymbolStream) : uint =
    match stream with
    | _ -> 0u

(* Expression patterns  *)

let (|Atom|) (stream : SymbolStream) : NodeTree =
    match stream with
    |   Symbol.Name(s, e, t) :: rest -> AST.Name(s, e, t), rest
    |   Symbol.Number(s, e, t) :: rest -> AST.Number(s, e, t), rest
    |   Symbol.Ellipsis(s, e) :: rest -> AST.Ellipsis(s, e), rest
    |   Symbol.None(s, e) :: rest -> AST.None(s, e), rest
    |   Symbol.False(s, e) :: rest -> AST.False(s, e), rest
    |   Symbol.True(s, e) :: rest -> AST.True(s, e), rest
    |   _ -> failwith "Expecting an expression value!"
    
let (|AwaitPrimary|) (stream : SymbolStream) : NodeTree =
    match stream with
    |   Symbol.Await(s, _) :: rest ->
            match rest with
            |   Atom(right, rest2) -> (AST.Await(s, GetEndPosition rest2, right), rest2)     
    |   Atom(ast, rest2) -> ast, rest2
    
    
    
let Parse(stream : SymbolStream) : NodeTree =
    match stream with
    | AwaitPrimary(ast, rest) -> ast, rest