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
    | Multiply of uint * uint
    | Divide of uint * uint
    | Modulo of uint * uint
    | FloorDivide of uint * uint
    | Matrices of uint * uint
    
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
    | Multiply of uint * uint * AST * AST
    | Divide of uint * uint * AST * AST
    | Modulo of uint * uint * AST * AST
    | FloorDivide of uint * uint * AST * AST
    | Matrices of uint * uint * AST * AST
    
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
    
let  (|Term|) (stream : SymbolStream) : NodeTree =
    let rec loop left symbols s =
        match symbols with
        |   Symbol.Multiply _ :: rest ->
                match rest with
                |   Factor(right, rest') ->
                        let left' = AST.Multiply(s, GetEndPosition rest', left, right)
                        loop left' rest' s
        |   Symbol.Divide _ :: rest ->
                match rest with
                |   Factor(right, rest') ->
                        let left' = AST.Divide(s, GetEndPosition rest', left, right)
                        loop left' rest' s
        |   Symbol.Modulo _ :: rest ->
                match rest with
                |   Factor(right, rest') ->
                        let left' = AST.Modulo(s, GetEndPosition rest', left, right)
                        loop left' rest' s
        |   Symbol.FloorDivide _ :: rest ->
                match rest with
                |   Factor(right, rest') ->
                        let left' = AST.FloorDivide(s, GetEndPosition rest', left, right)
                        loop left' rest' s
        |   Symbol.Matrices _ :: rest ->
                match rest with
                |   Factor(right, rest') ->
                        let left' = AST.Matrices(s, GetEndPosition rest', left, right)
                        loop left' rest' s
        |   _ -> left, symbols
        
    let s = GetStartPosition stream
    match stream with
    |   Factor(left, rest) -> loop left rest s
    
let  (|Sum|) (stream : SymbolStream) : NodeTree =
    AST.Empty, stream
    
let Parse(stream : SymbolStream) : NodeTree =
    match stream with
    | Term(ast, rest) -> ast, rest