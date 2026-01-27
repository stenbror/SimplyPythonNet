module SimplyPythonNet.ActivePatternParser


type Symbol =
    | Name of uint * uint * string
    | Number of uint * uint * string
    | String of uint * uint * string
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
    | Comma of uint * uint
    | Yield of uint * uint
    | From of uint * uint
    | If of uint * uint
    | Else of uint * uint
    | Lambda of uint * uint
    | As of uint * uint
    | With of uint * uint
    | Assert of uint * uint
    | Import of uint * uint
    | Global of uint * uint
    | Nonlocal of uint * uint
    | Pass of uint * uint
    | Break of uint * uint
    | Continue of uint * uint
    | Return of uint * uint
    | Raise of uint * uint
    | Try of uint * uint
    | Except of uint * uint
    | Finally of uint * uint
    | While of uint * uint
    | Del of uint * uint
    | Class of uint * uint
    | Def of uint * uint
    | For of uint * uint
    | Async of uint * uint
    | Elif of uint * uint
    | ShiftLeftAssign of uint * uint
    | ShiftRightAssign of uint * uint
    | PowerAssign of uint * uint
    | FloorDivideAssign of uint * uint
    | MatricesEqual of uint * uint
    | PlusEqual of uint * uint
    | MinusEqual of uint * uint
    | MultiplyEqual of uint * uint
    | DivideEqual of uint * uint
    | ModuloEqual of uint * uint
    | BitwiseAndEqual of uint * uint
    | BitwiseOrEqual of uint * uint
    | BitwiseXorEqual of uint * uint
    | Arrow of uint * uint
    | Colon of uint * uint
    | Period of uint * uint
    | LeftParenthesis of uint * uint
    | RightParenthesis of uint * uint
    | LeftSquareBracket of uint * uint
    | RightSquareBracket of uint * uint
    | LeftCurlyBracket of uint * uint
    | RightCurlyBracket of uint * uint
    | Assign of uint * uint
    
    
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
    | ExpressionList of uint * uint * AST list
    | StarNamedExpression of uint * uint * AST
    | StarNamedExpressionList of uint * uint * AST list
    | StarExpression of uint * uint * AST
    | StarExpressionList of uint * uint * AST list
    | YieldFrom of uint * uint * AST
    | Yield of uint * uint * AST
    | Test of uint * uint * AST * AST * AST
    
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

(* Tokenizer patterns *)

let (|LiteralStartCharacter|_|) (c: char) =
    if ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z') || c = '_' then Some(c) else Option.None

let (|LiteralNextCharacter|_|) (c: char) =
    if ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z') || c = '_' || ('0' <= c && c <= '9') then Some(c) else Option.None
    
let (|PrefixToString|_|) (text: char list) : (Symbol * char list) option =
    match text with
    |   'r' :: '\'' :: _ -> Some(Symbol.None(0u, 0u), text)
    |   'r' :: '"'  :: _ -> Some(Symbol.None(0u, 0u), text)
    |   'R' :: '\'' :: _ -> Some(Symbol.None(0u, 0u), text)
    |   'R' :: '"'  :: _ -> Some(Symbol.None(0u, 0u), text)
    |   'b' :: '\'' :: _ -> Some(Symbol.None(0u, 0u), text)
    |   'b' :: '"'  :: _ -> Some(Symbol.None(0u, 0u), text)
    |   'B' :: '\'' :: _ -> Some(Symbol.None(0u, 0u), text)
    |   'B' :: '"'  :: _ -> Some(Symbol.None(0u, 0u), text)
    |   'f' :: '\'' :: _ -> Some(Symbol.None(0u, 0u), text)
    |   'f' :: '"'  :: _ -> Some(Symbol.None(0u, 0u), text)
    |   'F' :: '\'' :: _ -> Some(Symbol.None(0u, 0u), text)
    |   'F' :: '"'  :: _ -> Some(Symbol.None(0u, 0u), text)
    |   't' :: '\'' :: _ -> Some(Symbol.None(0u, 0u), text)
    |   't' :: '"'  :: _ -> Some(Symbol.None(0u, 0u), text)
    |   'T' :: '\'' :: _ -> Some(Symbol.None(0u, 0u), text)
    |   'T' :: '"'  :: _ -> Some(Symbol.None(0u, 0u), text)
    |   'u' :: '\'' :: _ -> Some(Symbol.None(0u, 0u), text)
    |   'u' :: '"'  :: _ -> Some(Symbol.None(0u, 0u), text)
    |   'U' :: '\'' :: _ -> Some(Symbol.None(0u, 0u), text)
    |   'U' :: '"'  :: _ -> Some(Symbol.None(0u, 0u), text)
    |   'r' :: 'f':: '\'' :: _  -> Some(Symbol.None(0u, 0u), text)
    |   'r' :: 'f' :: '"' :: _  -> Some(Symbol.None(0u, 0u), text)
    |   'r' :: 'F':: '\'' :: _  -> Some(Symbol.None(0u, 0u), text)
    |   'r' :: 'F' :: '"' :: _  -> Some(Symbol.None(0u, 0u), text)
    |   'R' :: 'f':: '\'' :: _  -> Some(Symbol.None(0u, 0u), text)
    |   'R' :: 'f' :: '"' :: _  -> Some(Symbol.None(0u, 0u), text)
    |   'R' :: 'F':: '\'' :: _  -> Some(Symbol.None(0u, 0u), text)
    |   'R' :: 'F' :: '"' :: _  -> Some(Symbol.None(0u, 0u), text)   
    |   'f' :: 'r':: '\'' :: _  -> Some(Symbol.None(0u, 0u), text)
    |   'f' :: 'r' :: '"' :: _  -> Some(Symbol.None(0u, 0u), text)
    |   'F' :: 'r':: '\'' :: _  -> Some(Symbol.None(0u, 0u), text)
    |   'F' :: 'r' :: '"' :: _  -> Some(Symbol.None(0u, 0u), text)
    |   'f' :: 'R':: '\'' :: _  -> Some(Symbol.None(0u, 0u), text)
    |   'f' :: 'R' :: '"' :: _  -> Some(Symbol.None(0u, 0u), text)
    |   'F' :: 'R':: '\'' :: _  -> Some(Symbol.None(0u, 0u), text)
    |   'F' :: 'R' :: '"' :: _  -> Some(Symbol.None(0u, 0u), text)
    |   'r' :: 'b':: '\'' :: _  -> Some(Symbol.None(0u, 0u), text)
    |   'r' :: 'b' :: '"' :: _  -> Some(Symbol.None(0u, 0u), text)
    |   'r' :: 'B':: '\'' :: _  -> Some(Symbol.None(0u, 0u), text)
    |   'r' :: 'B' :: '"' :: _  -> Some(Symbol.None(0u, 0u), text)
    |   'R' :: 'b':: '\'' :: _  -> Some(Symbol.None(0u, 0u), text)
    |   'R' :: 'b' :: '"' :: _  -> Some(Symbol.None(0u, 0u), text)
    |   'R' :: 'B':: '\'' :: _  -> Some(Symbol.None(0u, 0u), text)
    |   'R' :: 'B' :: '"' :: _  -> Some(Symbol.None(0u, 0u), text)   
    |   'b' :: 'r':: '\'' :: _  -> Some(Symbol.None(0u, 0u), text)
    |   'b' :: 'r' :: '"' :: _  -> Some(Symbol.None(0u, 0u), text)
    |   'B' :: 'r':: '\'' :: _  -> Some(Symbol.None(0u, 0u), text)
    |   'B' :: 'r' :: '"' :: _  -> Some(Symbol.None(0u, 0u), text)
    |   'b' :: 'R':: '\'' :: _  -> Some(Symbol.None(0u, 0u), text)
    |   'b' :: 'R' :: '"' :: _  -> Some(Symbol.None(0u, 0u), text)
    |   'B' :: 'R':: '\'' :: _  -> Some(Symbol.None(0u, 0u), text)
    |   'B' :: 'R' :: '"' :: _  -> Some(Symbol.None(0u, 0u), text) 
    |   'r' :: 't':: '\'' :: _  -> Some(Symbol.None(0u, 0u), text)
    |   'r' :: 't' :: '"' :: _  -> Some(Symbol.None(0u, 0u), text)
    |   'r' :: 'T':: '\'' :: _  -> Some(Symbol.None(0u, 0u), text)
    |   'r' :: 'T' :: '"' :: _  -> Some(Symbol.None(0u, 0u), text)
    |   'R' :: 't':: '\'' :: _  -> Some(Symbol.None(0u, 0u), text)
    |   'R' :: 't' :: '"' :: _  -> Some(Symbol.None(0u, 0u), text)
    |   'R' :: 'T':: '\'' :: _  -> Some(Symbol.None(0u, 0u), text)
    |   'R' :: 'T' :: '"' :: _  -> Some(Symbol.None(0u, 0u), text)   
    |   't' :: 'r':: '\'' :: _  -> Some(Symbol.None(0u, 0u), text)
    |   't' :: 'r' :: '"' :: _  -> Some(Symbol.None(0u, 0u), text)
    |   'T' :: 'r':: '\'' :: _  -> Some(Symbol.None(0u, 0u), text)
    |   'T' :: 'r' :: '"' :: _  -> Some(Symbol.None(0u, 0u), text)
    |   't' :: 'R':: '\'' :: _  -> Some(Symbol.None(0u, 0u), text)
    |   't' :: 'R' :: '"' :: _  -> Some(Symbol.None(0u, 0u), text)
    |   'T' :: 'R':: '\'' :: _  -> Some(Symbol.None(0u, 0u), text)
    |   'T' :: 'R' :: '"' :: _  -> Some(Symbol.None(0u, 0u), text)
    |   _ ->
            Option.None

let (|ReservedKeywordOrLiteral|_|) (text: char list, start: uint) : (Symbol * char list) option =
    let rec loop acc rest =
        match rest with
        |   LiteralNextCharacter(c) :: rest2 -> loop (c :: acc) rest2
        |   _ ->
            match acc with
            |   [] -> Option.None
            |   letters -> Some(letters |> List.rev |> System.String.Concat, rest)
    
    match text with
    |   PrefixToString(symbol, rest) -> Some(symbol, rest)
    |   LiteralStartCharacter(c) :: rest ->
            match loop [] text with
            |   Some(keyword, rest2) ->
                    match keyword with
                    |   "False" -> Some(Symbol.False(start, start + 5u), rest2)
                    |   "True" -> Some(Symbol.True(start, start + 4u), rest2)
                    |   "None" -> Some(Symbol.None(start, start + 4u), rest2)
                    |   "and" -> Some(Symbol.And(start, start + 3u), rest2)
                    |   "as" -> Some(Symbol.As(start, start + 2u), rest2)
                    |   "assert" -> Some(Symbol.Assert(start, start + 6u), rest2)
                    |   "async" -> Some(Symbol.Async(start, start + 5u), rest2)
                    |   "await" -> Some(Symbol.Await(start, start + 5u), rest2)
                    |   "break" -> Some(Symbol.Break(start, start + 5u), rest2)
                    |   "class" -> Some(Symbol.Class(start, start + 5u), rest2)
                    |   "continue" -> Some(Symbol.Continue(start, start + 8u), rest2)
                    |   "def" -> Some(Symbol.Def(start, start + 3u), rest2)
                    |   "del" -> Some(Symbol.Del(start, start + 3u), rest2)
                    |   "elif" -> Some(Symbol.Elif(start, start + 4u), rest2)
                    |   "else" -> Some(Symbol.Else(start, start + 4u), rest2)
                    |   "except" -> Some(Symbol.Except(start, start + 6u), rest2)
                    |   "finally" -> Some(Symbol.Finally(start, start + 7u), rest2)
                    |   "for" -> Some(Symbol.For(start, start + 3u), rest2)
                    |   "from" -> Some(Symbol.From(start, start + 4u), rest2)
                    |   "global" -> Some(Symbol.Global(start, start + 6u), rest2)
                    |   "if" -> Some(Symbol.If(start, start + 2u), rest2)
                    |   "import" -> Some(Symbol.Import(start, start + 6u), rest2)
                    |   "in" -> Some(Symbol.In(start, start + 2u), rest2)
                    |   "is" -> Some(Symbol.Is(start, start + 2u), rest2)
                    |   "lambda" -> Some(Symbol.Lambda(start, start + 6u), rest2)
                    |   "nonlocal" -> Some(Symbol.Nonlocal(start, start + 7u), rest2)
                    |   "not" -> Some(Symbol.Not(start, start + 3u), rest2)
                    |   "or" -> Some(Symbol.Or(start, start + 2u), rest2)
                    |   "pass" -> Some(Symbol.Pass(start, start + 4u), rest2)
                    |   "raise" -> Some(Symbol.Raise(start, start + 5u), rest2)
                    |   "return" -> Some(Symbol.Return(start, start + 6u), rest2)
                    |   "try" -> Some(Symbol.Try(start, start + 3u), rest2)
                    |   "while" -> Some(Symbol.While(start, start + 5u), rest2)
                    |   "with" -> Some(Symbol.With(start, start + 4u), rest2)
                    |   "yield" -> Some(Symbol.Yield(start, start + 5u), rest2)                  
                    |   _ -> Some(Symbol.Name(start, start + uint keyword.Length, keyword), rest2)
            |   _ -> Option.None
    |   _ -> Option.None
    
let (|OperatorOrDelimiter|_|) (text: char list, start: uint) : (Symbol * char list) option =
    match text with
    |   '<' :: '<' :: '=' :: rest -> Some(Symbol.ShiftLeftAssign(start, start + 3u), rest)
    |   '>' :: '>' :: '=' :: rest -> Some(Symbol.ShiftRightAssign(start, start + 3u), rest)
    |   '*':: '*' :: '=' :: rest -> Some(Symbol.PowerAssign(start, start + 3u), rest)
    |   '/' :: '/' :: '=' :: rest -> Some(Symbol.FloorDivideAssign(start, start + 3u), rest)
    |   '.' :: '.' :: '.' :: rest -> Some(Symbol.Ellipsis(start, start + 3u), rest)
    |   '+' :: '=' :: rest -> Some(Symbol.PlusEqual(start, start + 2u), rest)
    |   '-' :: '=' :: rest -> Some(Symbol.MinusEqual(start, start + 2u), rest)
    |   '*' :: '=' :: rest -> Some(Symbol.MultiplyEqual(start, start + 2u), rest)
    |   '/' :: '=' :: rest -> Some(Symbol.DivideEqual(start, start + 2u), rest)
    |   '%' :: '=' :: rest -> Some(Symbol.ModuloEqual(start, start + 2u), rest)
    |   '&' :: '=' :: rest -> Some(Symbol.BitwiseAndEqual(start, start + 2u), rest)
    |   '|' :: '=' :: rest -> Some(Symbol.BitwiseOrEqual(start, start + 2u), rest)
    |   '^' :: '=' :: rest -> Some(Symbol.BitwiseXorEqual(start, start + 2u), rest)
    |   '-' :: '>' :: rest -> Some(Symbol.Arrow(start, start + 2u), rest)
    |   '/' :: '/' :: rest -> Some(Symbol.FloorDivide(start, start + 2u), rest)
    |   '<' :: '=' :: rest -> Some(Symbol.LessOrEqual(start, start + 2u), rest)
    |   '>' :: '=' :: rest -> Some(Symbol.GreaterOrEqual(start, start + 2u), rest)
    |   '!' :: '=' :: rest -> Some(Symbol.NotEqual(start, start + 2u), rest)
    |   '@' :: '=' :: rest -> Some(Symbol.MatricesEqual(start, start + 2u), rest)
    |   ':' :: '=' :: rest -> Some(Symbol.ColonEqual(start, start + 2u), rest)
    |   '=' :: '=' :: rest -> Some(Symbol.Equal(start, start + 2u), rest)
    |   '*' :: '*' :: rest -> Some(Symbol.Power(start, start + 2u), rest)
    |   '+' :: rest -> Some(Symbol.Plus(start, start + 1u), rest)
    |   '-' :: rest -> Some(Symbol.Minus(start, start + 1u), rest)
    |   '/' :: rest -> Some(Symbol.Divide(start, start + 1u), rest)
    |   '%' :: rest -> Some(Symbol.Modulo(start, start + 1u), rest)
    |   '&' :: rest -> Some(Symbol.BitwiseAnd(start, start + 1u), rest)
    |   '*' :: rest -> Some(Symbol.Multiply(start, start + 1u), rest)
    |   '|' :: rest -> Some(Symbol.BitwiseOr(start, start + 1u), rest)
    |   '^' :: rest -> Some(Symbol.BitwiseXor(start, start + 1u), rest)
    |   '@' :: rest -> Some(Symbol.Matrices(start, start + 1u), rest)
    |   '!' :: rest -> Some(Symbol.Not(start, start + 1u), rest)
    |   ':' :: rest -> Some(Symbol.Colon(start, start + 1u), rest)
    |   ';' :: rest -> Some(Symbol.SemiColon(start, start + 1u), rest)
    |   ',' :: rest -> Some(Symbol.Comma(start, start + 1u), rest)
    |   '.' :: rest -> Some(Symbol.Period(start, start + 1u), rest)
    |   '(' :: rest -> Some(Symbol.LeftParenthesis(start, start + 1u), rest)
    |   ')' :: rest -> Some(Symbol.RightParenthesis(start, start + 1u), rest)
    |   '[' :: rest -> Some(Symbol.LeftSquareBracket(start, start + 1u), rest)
    |   ']' :: rest -> Some(Symbol.RightSquareBracket(start, start + 1u), rest)
    |   '{' :: rest -> Some(Symbol.LeftCurlyBracket(start, start + 1u), rest)
    |   '}' :: rest -> Some(Symbol.RightCurlyBracket(start, start + 1u), rest)
    |   '~' :: rest -> Some(Symbol.BitwiseInvert(start, start + 1u), rest)
    |   '=' :: rest -> Some(Symbol.Assign(start, start + 1u), rest)
    |   '<' :: rest -> Some(Symbol.Less(start, start + 1u), rest)
    |   '>' :: rest -> Some(Symbol.Greater(start, start + 1u), rest)
    |   _ ->    Option.None
    
let (|SingleOrTripleString|_|) (text: char list, start: uint) : (Symbol * char list) option =
    match text, start with
    |   _ -> Option.None
    
let (|Number|_|) (text: char list, start: uint) : (Symbol * char list) option =
    match text, start with
    |   _ -> Option.None
    
    
let Tokenize(text: char list) : SymbolStream =
    let size = uint(text.Length)
    let mutable elements : SymbolStream = []
    let mutable source = text
    
    while source.Length > 0 do
        
        while match source with
              | '\t' :: rest | ' ' :: rest ->
                    source <- rest
                    true
              | _ -> false
              do ()
        
        let mutable pos = size - uint(source.Length)
           
        match source, pos with
        |   Number(s, r) ->
                elements <- s :: elements
                source <- r
        |   OperatorOrDelimiter(s, r) ->
                elements <- s :: elements
                source <- r
        |   ReservedKeywordOrLiteral(s, r) ->
                elements <- s :: elements
                source <- r
        |   SingleOrTripleString(s, r) ->
                elements <- s :: elements
                source <- r
        |   _ -> failwith "Unknown symbol!"
    
    List.rev elements

(* Expression patterns  *)

let rec (|Atom|) (stream : SymbolStream) : NodeTree =
    match stream with
    |   Symbol.Name(s, e, t) :: rest -> AST.Name(s, e, t), rest
    |   Symbol.Number(s, e, t) :: rest -> AST.Number(s, e, t), rest
    |   Symbol.Ellipsis(s, e) :: rest -> AST.Ellipsis(s, e), rest
    |   Symbol.None(s, e) :: rest -> AST.None(s, e), rest
    |   Symbol.False(s, e) :: rest -> AST.False(s, e), rest
    |   Symbol.True(s, e) :: rest -> AST.True(s, e), rest
    |   Strings(s, e) -> s, e
    |   TupleOrGeneratorExpression(s, e) -> s, e
    |   ListOrListComp(s, e) -> s, e
    |   DictionaryOrSet(s, e) -> s, e
    |   _ -> failwith "Expecting an expression value!"
    
and (|Strings|_|) (stream : SymbolStream) : NodeTree option =
    let rec loop acc tokens =
        match tokens with
        | Symbol.String(_, _,  t) :: rest ->
            loop (t :: acc) rest
        | _ ->
            List.rev acc, tokens
            
    match stream with
    |   Symbol.String(s, _, _) :: _ -> 
            let res, restFinal = loop [] stream
            Some(AST.String(s, GetStartPosition restFinal, res), restFinal)
    |   _ -> Option.None
    
and (|TupleOrGeneratorExpression|_|) (stream : SymbolStream) : NodeTree option = Option.None

and (|ListOrListComp|_|) (stream : SymbolStream) : NodeTree option = Option.None

and (|DictionaryOrSet|_|) (stream : SymbolStream) : NodeTree option = Option.None
              
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
    
and  (|StarNamedExpression|) (stream : SymbolStream) : NodeTree =
    let s = GetStartPosition stream
    match stream with
    | Symbol.Multiply _ :: rest ->
        match rest with
        | BitwiseOr(ast, rest2) ->
            AST.StarNamedExpression(s, GetStartPosition rest2, ast), rest2
    |   NamedExpression(ast, rest) -> ast, rest
    
and  (|StarNamedExpressions|) (stream : SymbolStream) : NodeTree =
    let s = GetStartPosition stream
    match stream with
    |   StarNamedExpression(ast, rest1) ->
            let rec loop acc tokens =
                match tokens with
                |   Symbol.Comma _ :: Symbol.In _ :: _ -> List.rev acc, tokens.Tail
                |   Symbol.Comma _ :: StarNamedExpression(expr, rest2) -> loop (expr :: acc) rest2
                |   _ -> List.rev acc, tokens
            let expr, restFinal = loop [ast] rest1
            match expr.Length with
            | 1 -> ast, restFinal
            | _ -> AST.StarNamedExpressionList(s, GetEndPosition restFinal, expr), restFinal
            
and  (|StarExpression|) (stream : SymbolStream) : NodeTree =
    let s = GetStartPosition stream
    match stream with
    | Symbol.Multiply _ :: rest ->
        match rest with
        | BitwiseOr(ast, rest2) ->
            AST.StarExpression(s, GetStartPosition rest2, ast), rest2
    |   Expression(ast, rest) -> ast, rest
    
and  (|StarExpressions|) (stream : SymbolStream) : NodeTree =
    let s = GetStartPosition stream
    match stream with
    |   StarExpression(ast, rest1) ->
            let rec loop acc tokens =
                match tokens with
                |   Symbol.Comma _ :: Symbol.In _ :: _ -> List.rev acc, tokens.Tail
                |   Symbol.Comma _ :: StarExpression(expr, rest2) -> loop (expr :: acc) rest2
                |   _ -> List.rev acc, tokens
            let expr, restFinal = loop [ast] rest1
            match expr.Length with
            | 1 -> ast, restFinal
            | _ -> AST.StarExpressionList(s, GetEndPosition restFinal, expr), restFinal

and  (|YieldExpression|_|) (stream : SymbolStream) : NodeTree option =
    let s = GetStartPosition stream
    match stream with
    | Symbol.Yield _ :: Symbol.From _ :: rest ->
        match rest with
        | Expression(ast, rest2) -> Some(AST.YieldFrom(s, GetEndPosition rest2, ast), rest2)
    | Symbol.Yield _ :: rest ->
        match rest with
        | StarExpression(ast, rest2) -> Some(AST.Yield(s, GetEndPosition rest2, ast), rest2)
    | _ -> Option.None

and  (|Expression|) (stream : SymbolStream) : NodeTree =
    let s = GetStartPosition stream
    match stream with
    |   LambdaDef(ast, rest) -> ast, rest
    |   Disjunction(ast, rest) ->
            match rest with
            |   Symbol.If _ :: rest2 ->
                    match rest2 with
                    |   Disjunction(cond, rest3) ->
                        match rest3 with
                        |   Symbol.Else _ :: rest4 ->
                            match rest4 with
                            |   Expression(body, rest5) -> AST.Test(s, GetEndPosition rest5, ast, cond, body), rest5
                        |   _ -> failwith "Expecting 'else' in expression"
            |    _ -> ast, rest
    
and  (|Expressions|) (stream : SymbolStream) : NodeTree =
    let s = GetStartPosition stream
    match stream with
    |   Expression(ast, rest1) ->
            let rec loop acc tokens =
                match tokens with
                |   Symbol.Comma _ :: Symbol.In _ :: _ -> List.rev acc, tokens.Tail
                |   Symbol.Comma _ :: Expression(expr, rest2) -> loop (expr :: acc) rest2
                |   _ -> List.rev acc, tokens
            let expr, restFinal = loop [ast] rest1
            match expr.Length with
            | 1 -> ast, restFinal
            | _ -> AST.ExpressionList(s, GetEndPosition restFinal, expr), restFinal
            
            
and  (|LambdaDef|_|) (stream : SymbolStream) : NodeTree option =
    match stream with
    |   Symbol.Lambda _ :: rest ->
            Some(AST.Empty, rest)
    |   _ -> Option.None
    

let Parse(stream : SymbolStream) : NodeTree =
    match stream with
    | Expression(ast, rest) -> ast, rest
