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
    | SemiColon of uint * uint
    | ShiftLeft of uint * uint
    | ShiftRight of uint * uint
    | BitwiseAnd of uint * uint
    | BitwiseOr of uint * uint
    | BitwiseXor of uint * uint
    | Less of uint * uint
    | Greater of uint * uint
    | Equal of uint * uint
    | NotEqual of uint * uint
    | LessOrEqual of uint * uint
    | GreaterOrEqual of uint * uint
    | Is of uint * uint
    | Not of uint * uint
    | In of uint * uint
    | And of uint * uint
    | Or of uint * uint
    | ColonEqual of uint * uint
    
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
    | Plus of uint * uint * AST * AST
    | Minus of uint * uint * AST * AST
    | ShiftLeft of uint * uint * AST * AST
    | ShiftRight of uint * uint * AST * AST
    | BitwiseAnd of uint * uint * AST * AST
    | BitwiseOr of uint * uint * AST * AST
    | BitwiseXor of uint * uint * AST * AST
    | Less of uint * uint * AST * AST
    | Greater of uint * uint * AST * AST
    | Equal of uint * uint * AST * AST
    | NotEqual of uint * uint * AST * AST
    | LessOrEqual of uint * uint * AST * AST
    | GreaterOrEqual of uint * uint * AST * AST
    | Is of uint * uint * AST * AST
    | IsNot of uint * uint * AST * AST
    | In of uint * uint * AST * AST
    | NotIn of uint * uint * AST * AST
    | Not_ of uint * uint * AST
    | And of uint * uint * AST * AST
    | Or of uint * uint * AST * AST
    | NamedAssignment of uint * uint * AST * AST
    
type SymbolStream = Symbol list

type NodeTree = AST * SymbolStream

(* Utilities functions *)

let GetStartPosition (stream : SymbolStream) : uint =
    match stream with
    | Symbol.SemiColon(s, _) :: _ -> s
    | Symbol.Name(s, _, _) :: _ -> s
    | Symbol.Number(s, _, _) :: _ -> s
    | Symbol.Plus(s, _) :: _ -> s
    | Symbol.Minus(s, _) :: _ -> s
    | _ -> 0u
    
let GetEndPosition (stream : SymbolStream) : uint =
    match stream with
    | Symbol.SemiColon(_, e) :: _ -> e
    | _ -> 0u

(* Expression patterns  *)

let rec (|Atom|) (stream : SymbolStream) : NodeTree =
    match stream with
    |   Symbol.Name(s, e, t) :: rest -> AST.Name(s, e, t), rest
    |   Symbol.Number(s, e, t) :: rest -> AST.Number(s, e, t), rest
    |   Symbol.Ellipsis(s, e) :: rest -> AST.Ellipsis(s, e), rest
    |   Symbol.None(s, e) :: rest -> AST.None(s, e), rest
    |   Symbol.False(s, e) :: rest -> AST.False(s, e), rest
    |   Symbol.True(s, e) :: rest -> AST.True(s, e), rest
    |   _ -> failwith "Expecting an expression value!"
    
and (|Primary|) (stream: SymbolStream) : NodeTree =
    match stream with
    |   Atom(ast, rest2) -> ast, rest2
    
and (|AwaitPrimary|) (stream : SymbolStream) : NodeTree =
    match stream with
    |   Symbol.Await(s, _) :: rest ->
            match rest with
            |   Atom(right, rest2) -> (AST.Await(s, GetEndPosition rest2, right), rest2)     
    |   Primary(ast, rest2) -> ast, rest2
    
and (|Power|) (stream : SymbolStream) : NodeTree =
    let s = GetStartPosition stream
    let left, rest = match stream with |   AwaitPrimary(ast, rest2) -> ast, rest2
    match rest with
    |   Symbol.Power _ :: rest3 ->
            match rest3 with
            |   AwaitPrimary(right, rest4) -> (AST.Power(s, (GetEndPosition rest4), left, right), rest4)     
    |   _ -> left, rest
    
and (|Factor|) (stream : SymbolStream) : NodeTree =
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
    
and  (|Term|) (stream : SymbolStream) : NodeTree =
    let rec loop left symbols s =
        match symbols with
        |   Symbol.Multiply _ :: rest ->
                match rest with
                |   Factor(right, rest') ->
                        let left' = AST.Multiply(s, GetStartPosition rest', left, right)
                        loop left' rest' s
        |   Symbol.Divide _ :: rest ->
                match rest with
                |   Factor(right, rest') ->
                        let left' = AST.Divide(s, GetStartPosition rest', left, right)
                        loop left' rest' s
        |   Symbol.Modulo _ :: rest ->
                match rest with
                |   Factor(right, rest') ->
                        let left' = AST.Modulo(s, GetStartPosition rest', left, right)
                        loop left' rest' s
        |   Symbol.FloorDivide _ :: rest ->
                match rest with
                |   Factor(right, rest') ->
                        let left' = AST.FloorDivide(s, GetStartPosition rest', left, right)
                        loop left' rest' s
        |   Symbol.Matrices _ :: rest ->
                match rest with
                |   Factor(right, rest') ->
                        let left' = AST.Matrices(s, GetStartPosition rest', left, right)
                        loop left' rest' s
        |   _ -> left, symbols
        
    let s = GetStartPosition stream
    match stream with
    |   Factor(left, rest) -> loop left rest s
    
and  (|Sum|) (stream : SymbolStream) : NodeTree =
    let rec loop left symbols s =
        match symbols with
        |   Symbol.Plus _ :: rest ->
                match rest with
                |   Term(right, rest') ->
                        let left' = AST.Plus(s, GetStartPosition rest', left, right)
                        loop left' rest' s
        |   Symbol.Minus _ :: rest ->
                match rest with
                |   Term(right, rest') ->
                        let left' = AST.Minus(s, GetStartPosition rest', left, right)
                        loop left' rest' s
        |   _ -> left, symbols
        
    let s = GetStartPosition stream
    match stream with
    |   Term(left, rest) -> loop left rest s
    
and  (|Shift|) (stream : SymbolStream) : NodeTree =
    let rec loop left symbols s =
        match symbols with
        |   Symbol.ShiftLeft _ :: rest ->
                match rest with
                |   Sum(right, rest') ->
                        let left' = AST.ShiftLeft(s, GetStartPosition rest', left, right)
                        loop left' rest' s
        |   Symbol.ShiftRight _ :: rest ->
                match rest with
                |   Sum(right, rest') ->
                        let left' = AST.ShiftRight(s, GetStartPosition rest', left, right)
                        loop left' rest' s
        |   _ -> left, symbols
        
    let s = GetStartPosition stream
    match stream with
    |   Sum(left, rest) -> loop left rest s
    
and  (|BitwiseAnd|) (stream : SymbolStream) : NodeTree =
    let rec loop left symbols s =
        match symbols with
        |   Symbol.BitwiseAnd _ :: rest ->
                match rest with
                |   Shift(right, rest') ->
                        let left' = AST.BitwiseAnd(s, GetStartPosition rest', left, right)
                        loop left' rest' s
        |   _ -> left, symbols
        
    let s = GetStartPosition stream
    match stream with
    |   Shift(left, rest) -> loop left rest s
    
and  (|BitwiseXor|) (stream : SymbolStream) : NodeTree =
    let rec loop left symbols s =
        match symbols with
        |   Symbol.BitwiseXor _ :: rest ->
                match rest with
                |   BitwiseAnd(right, rest') ->
                        let left' = AST.BitwiseXor(s, GetStartPosition rest', left, right)
                        loop left' rest' s
        |   _ -> left, symbols
        
    let s = GetStartPosition stream
    match stream with
    |   BitwiseAnd(left, rest) -> loop left rest s
    
and  (|BitwiseOr|) (stream : SymbolStream) : NodeTree =
    let rec loop left symbols s =
        match symbols with
        |   Symbol.BitwiseOr _ :: rest ->
                match rest with
                |   BitwiseXor(right, rest') ->
                        let left' = AST.BitwiseOr(s, GetStartPosition rest', left, right)
                        loop left' rest' s
        |   _ -> left, symbols
        
    let s = GetStartPosition stream
    match stream with
    |   BitwiseXor(left, rest) -> loop left rest s
    
and  (|Comparison|) (stream : SymbolStream) : NodeTree =
    let rec loop left symbols s =
        match symbols with
        |   Symbol.Less _ :: rest ->
                match rest with
                |   BitwiseOr(right, rest') ->
                        let left' = AST.Less(s, GetStartPosition rest', left, right)
                        loop left' rest' s
        |   Symbol.Greater _ :: rest ->
                match rest with
                |   BitwiseOr(right, rest') ->
                        let left' = AST.Greater(s, GetStartPosition rest', left, right)
                        loop left' rest' s
        |   Symbol.Equal _ :: rest ->
                match rest with
                |   BitwiseOr(right, rest') ->
                        let left' = AST.Equal(s, GetStartPosition rest', left, right)
                        loop left' rest' s
        |   Symbol.NotEqual _ :: rest ->
                match rest with
                |   BitwiseOr(right, rest') ->
                        let left' = AST.NotEqual(s, GetStartPosition rest', left, right)
                        loop left' rest' s
        |   Symbol.LessOrEqual _ :: rest ->
                match rest with
                |   BitwiseOr(right, rest') ->
                        let left' = AST.LessOrEqual(s, GetStartPosition rest', left, right)
                        loop left' rest' s
        |   Symbol.GreaterOrEqual _ :: rest ->
                match rest with
                |   BitwiseOr(right, rest') ->
                        let left' = AST.GreaterOrEqual(s, GetStartPosition rest', left, right)
                        loop left' rest' s
        |   Symbol.In _ :: rest ->
                match rest with
                |   BitwiseOr(right, rest') ->
                        let left' = AST.In(s, GetStartPosition rest', left, right)
                        loop left' rest' s
        |   Symbol.Is _ :: rest ->
                match rest with
                |   Symbol.Not _ :: rest2 ->
                        match rest2 with
                        |   BitwiseOr(right, rest3') ->
                                let left' = AST.IsNot(s, GetStartPosition rest3', left, right)
                                loop left' rest3' s
                |   BitwiseOr(right, rest') ->
                        let left' = AST.Is(s, GetStartPosition rest', left, right)
                        loop left' rest' s
        |   Symbol.Not _ :: rest ->
                match rest with
                |   Symbol.In _ :: rest2 ->
                        match rest2 with
                        |   BitwiseOr(right, rest3') ->
                                let left' = AST.NotIn(s, GetStartPosition rest3', left, right)
                                loop left' rest3' s
                |   _ -> failwith "Expecting 'in' after 'not' in expression" 
        |   _ -> left, symbols
        
    let s = GetStartPosition stream
    match stream with
    |   BitwiseOr(left, rest) -> loop left rest s
    
and (|Inversion|) (stream : SymbolStream) : NodeTree =
    match stream with
    |   Symbol.Not(s, _) :: rest ->
            match rest with
            |   Inversion(right, rest2) -> (AST.Not_(s, GetEndPosition rest2, right), rest2) 
    |   Comparison(ast, rest2) -> ast, rest2
    
and  (|Conjunction|) (stream : SymbolStream) : NodeTree =
    let rec loop left symbols s =
        match symbols with
        |   Symbol.And _ :: rest ->
                match rest with
                |   Inversion(right, rest') ->
                        let left' = AST.And(s, GetStartPosition rest', left, right)
                        loop left' rest' s
        |   _ -> left, symbols
        
    let s = GetStartPosition stream
    match stream with
    |   Inversion(left, rest) -> loop left rest s
    
and  (|Disjunction|) (stream : SymbolStream) : NodeTree =
    let rec loop left symbols s =
        match symbols with
        |   Symbol.Or _ :: rest ->
                match rest with
                |   Conjunction(right, rest') ->
                        let left' = AST.Or(s, GetStartPosition rest', left, right)
                        loop left' rest' s
        |   _ -> left, symbols
        
    let s = GetStartPosition stream
    match stream with
    |   Conjunction(left, rest) -> loop left rest s
    
and  (|NamedExpression|) (stream : SymbolStream) : NodeTree =
    match stream with
    |   Symbol.Name(s, e, t) :: Symbol.ColonEqual _ :: rest ->
            match rest with
            |   Expression(ast, rest2) -> AST.NamedAssignment(s, GetEndPosition rest2, AST.Name(s, e, t), ast), rest2
    |   Expression(ast, rest) -> ast, rest

    

and  (|Expression|) (stream : SymbolStream) : NodeTree =
    let s = GetStartPosition stream
    match stream with
    //|   Lambda(ast, rest) -> ast, rest
    |   Disjunction(ast, rest) -> ast, rest
    

let Parse(stream : SymbolStream) : NodeTree =
    match stream with
    | Expression(ast, rest) -> ast, rest