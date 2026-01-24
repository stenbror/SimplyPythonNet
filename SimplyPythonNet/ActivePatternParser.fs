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
    | Minus of uint * uint
    | BitwiseInvert of uint * uint
    | Power of uint * uint
    
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
    | Power of uint * uint * AST * AST
    | UnaryPlus of uint * uint * AST
    | UnaryMinus of uint * uint * AST
    | BitwiseInvert of uint * uint * AST
    
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
    
let (|Primary|) (stream: SymbolStream) : NodeTree =
    match stream with
    |   Atom(ast, rest2) -> ast, rest2
    
let (|AwaitPrimary|) (stream : SymbolStream) : NodeTree =
    match stream with
    |   Symbol.Await(s, _) :: rest ->
            match rest with
            |   Atom(right, rest2) -> (AST.Await(s, GetEndPosition rest2, right), rest2)     
    |   Primary(ast, rest2) -> ast, rest2
    
let (|Power|) (stream : SymbolStream) : NodeTree =
    let s = GetStartPosition stream
    let left, rest = match stream with |   AwaitPrimary(ast, rest2) -> ast, rest2
    match rest with
    |   Symbol.Power _ :: rest3 ->
            match rest3 with
            |   AwaitPrimary(right, rest4) -> (AST.Power(s, (GetEndPosition rest4), left, right), rest4)     
    |   _ -> left, rest
    
let rec (|Factor|) (stream : SymbolStream) : NodeTree =
    match stream with
    |   Symbol.Plus(s, _) :: rest ->
            match rest with
            |   Factor(right, rest2) -> (AST.UnaryPlus(s, GetEndPosition rest2, right), rest2)
    |   Symbol.Minus(s, _) :: rest ->
            match rest with
            |   Factor(right, rest2) -> (AST.UnaryMinus(s, GetEndPosition rest2, right), rest2)
    |   Symbol.BitwiseInvert(s, _) :: rest ->
            match rest with
            |   Factor(right, rest2) -> (AST.BitwiseInvert(s, GetEndPosition rest2, right), rest2)     
    |   Power(ast, rest2) -> ast, rest2
    
let Parse(stream : SymbolStream) : NodeTree =
    match stream with
    | Factor(ast, rest) -> ast, rest