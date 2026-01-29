module SimplyPythonNet.ActivePatternParser


type Symbol =
    | Newline of uint
    | Indent of uint
    | Dedent of uint
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
    
let (|PrefixToString|_|) (text: char list) : bool option =
    match text with
    |   'r' :: '\'' :: _ -> Some(true)
    |   'r' :: '"'  :: _ -> Some(true)
    |   'R' :: '\'' :: _ -> Some(true)
    |   'R' :: '"'  :: _ -> Some(true)
    |   'b' :: '\'' :: _ -> Some(true)
    |   'b' :: '"'  :: _ -> Some(true)
    |   'B' :: '\'' :: _ -> Some(true)
    |   'B' :: '"'  :: _ -> Some(true)
    |   'f' :: '\'' :: _ -> Some(true)
    |   'f' :: '"'  :: _ -> Some(true)
    |   'F' :: '\'' :: _ -> Some(true)
    |   'F' :: '"'  :: _ -> Some(true)
    |   't' :: '\'' :: _ -> Some(true)
    |   't' :: '"'  :: _ -> Some(true)
    |   'T' :: '\'' :: _ -> Some(true)
    |   'T' :: '"'  :: _ -> Some(true)
    |   'u' :: '\'' :: _ -> Some(true)
    |   'u' :: '"'  :: _ -> Some(true)
    |   'U' :: '\'' :: _ -> Some(true)
    |   'U' :: '"'  :: _ -> Some(true)
    |   'r' :: 'f':: '\'' :: _  -> Some(true)
    |   'r' :: 'f' :: '"' :: _  -> Some(true)
    |   'r' :: 'F':: '\'' :: _  -> Some(true)
    |   'r' :: 'F' :: '"' :: _  -> Some(true)
    |   'R' :: 'f':: '\'' :: _  -> Some(true)
    |   'R' :: 'f' :: '"' :: _  -> Some(true)
    |   'R' :: 'F':: '\'' :: _  -> Some(true)
    |   'R' :: 'F' :: '"' :: _  -> Some(true)   
    |   'f' :: 'r':: '\'' :: _  -> Some(true)
    |   'f' :: 'r' :: '"' :: _  -> Some(true)
    |   'F' :: 'r':: '\'' :: _  -> Some(true)
    |   'F' :: 'r' :: '"' :: _  -> Some(true)
    |   'f' :: 'R':: '\'' :: _  -> Some(true)
    |   'f' :: 'R' :: '"' :: _  -> Some(true)
    |   'F' :: 'R':: '\'' :: _  -> Some(true)
    |   'F' :: 'R' :: '"' :: _  -> Some(true)
    |   'r' :: 'b':: '\'' :: _  -> Some(true)
    |   'r' :: 'b' :: '"' :: _  -> Some(true)
    |   'r' :: 'B':: '\'' :: _  -> Some(true)
    |   'r' :: 'B' :: '"' :: _  -> Some(true)
    |   'R' :: 'b':: '\'' :: _  -> Some(true)
    |   'R' :: 'b' :: '"' :: _  -> Some(true)
    |   'R' :: 'B':: '\'' :: _  -> Some(true)
    |   'R' :: 'B' :: '"' :: _  -> Some(true)   
    |   'b' :: 'r':: '\'' :: _  -> Some(true)
    |   'b' :: 'r' :: '"' :: _  -> Some(true)
    |   'B' :: 'r':: '\'' :: _  -> Some(true)
    |   'B' :: 'r' :: '"' :: _  -> Some(true)
    |   'b' :: 'R':: '\'' :: _  -> Some(true)
    |   'b' :: 'R' :: '"' :: _  -> Some(true)
    |   'B' :: 'R':: '\'' :: _  -> Some(true)
    |   'B' :: 'R' :: '"' :: _  -> Some(true) 
    |   'r' :: 't':: '\'' :: _  -> Some(true)
    |   'r' :: 't' :: '"' :: _  -> Some(true)
    |   'r' :: 'T':: '\'' :: _  -> Some(true)
    |   'r' :: 'T' :: '"' :: _  -> Some(true)
    |   'R' :: 't':: '\'' :: _  -> Some(true)
    |   'R' :: 't' :: '"' :: _  -> Some(true)
    |   'R' :: 'T':: '\'' :: _  -> Some(true)
    |   'R' :: 'T' :: '"' :: _  -> Some(true)   
    |   't' :: 'r':: '\'' :: _  -> Some(true)
    |   't' :: 'r' :: '"' :: _  -> Some(true)
    |   'T' :: 'r':: '\'' :: _  -> Some(true)
    |   'T' :: 'r' :: '"' :: _  -> Some(true)
    |   't' :: 'R':: '\'' :: _  -> Some(true)
    |   't' :: 'R' :: '"' :: _  -> Some(true)
    |   'T' :: 'R':: '\'' :: _  -> Some(true)
    |   'T' :: 'R' :: '"' :: _  -> Some(true)
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
    
    let mutable prefix_seen = false
    match text with
    |   PrefixToString(res) ->
            prefix_seen <- res
            Option.None
    |   LiteralStartCharacter _  :: _ ->
            match prefix_seen with
            |   false ->
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
            |   true -> Option.None
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


let (|Prefix|_|) (text: char list) : (string * char list) option =
    match text with
    |   'r' :: '\'' :: _ 
    |   'r' :: '"'  :: _ 
    |   'R' :: '\'' :: _ 
    |   'R' :: '"'  :: _ -> Some("r", text.Tail)
    |   'b' :: '\'' :: _ 
    |   'b' :: '"'  :: _ 
    |   'B' :: '\'' :: _ 
    |   'B' :: '"'  :: _ -> Some("b", text.Tail)
    |   'f' :: '\'' :: _ 
    |   'f' :: '"'  :: _ 
    |   'F' :: '\'' :: _ 
    |   'F' :: '"'  :: _ -> Some("f", text.Tail)
    |   't' :: '\'' :: _ 
    |   't' :: '"'  :: _ 
    |   'T' :: '\'' :: _ 
    |   'T' :: '"'  :: _ -> Some("t", text.Tail)
    |   'u' :: '\'' :: _ 
    |   'u' :: '"'  :: _ 
    |   'U' :: '\'' :: _ 
    |   'U' :: '"'  :: _ -> Some("u", text.Tail)
    |   'r' :: 'f':: '\'' :: _  
    |   'r' :: 'f' :: '"' :: _  
    |   'r' :: 'F':: '\'' :: _  
    |   'r' :: 'F' :: '"' :: _  
    |   'R' :: 'f':: '\'' :: _  
    |   'R' :: 'f' :: '"' :: _  
    |   'R' :: 'F':: '\'' :: _  
    |   'R' :: 'F' :: '"' :: _  
    |   'f' :: 'r':: '\'' :: _  
    |   'f' :: 'r' :: '"' :: _  
    |   'F' :: 'r':: '\'' :: _  
    |   'F' :: 'r' :: '"' :: _  
    |   'f' :: 'R':: '\'' :: _  
    |   'f' :: 'R' :: '"' :: _  
    |   'F' :: 'R':: '\'' :: _  
    |   'F' :: 'R' :: '"' :: _  -> Some("rf", text.Tail.Tail)
    |   'r' :: 'b':: '\'' :: _  
    |   'r' :: 'b' :: '"' :: _  
    |   'r' :: 'B':: '\'' :: _  
    |   'r' :: 'B' :: '"' :: _  
    |   'R' :: 'b':: '\'' :: _  
    |   'R' :: 'b' :: '"' :: _  
    |   'R' :: 'B':: '\'' :: _  
    |   'R' :: 'B' :: '"' :: _  
    |   'b' :: 'r':: '\'' :: _  
    |   'b' :: 'r' :: '"' :: _  
    |   'B' :: 'r':: '\'' :: _  
    |   'B' :: 'r' :: '"' :: _  
    |   'b' :: 'R':: '\'' :: _  
    |   'b' :: 'R' :: '"' :: _  
    |   'B' :: 'R':: '\'' :: _  
    |   'B' :: 'R' :: '"' :: _  -> Some("br", text.Tail.Tail) 
    |   'r' :: 't':: '\'' :: _  
    |   'r' :: 't' :: '"' :: _  
    |   'r' :: 'T':: '\'' :: _  
    |   'r' :: 'T' :: '"' :: _  
    |   'R' :: 't':: '\'' :: _  
    |   'R' :: 't' :: '"' :: _  
    |   'R' :: 'T':: '\'' :: _  
    |   'R' :: 'T' :: '"' :: _  
    |   't' :: 'r':: '\'' :: _  
    |   't' :: 'r' :: '"' :: _ 
    |   'T' :: 'r':: '\'' :: _  
    |   'T' :: 'r' :: '"' :: _  
    |   't' :: 'R':: '\'' :: _  
    |   't' :: 'R' :: '"' :: _  
    |   'T' :: 'R':: '\'' :: _  
    |   'T' :: 'R' :: '"' :: _  -> Some("rt", text.Tail.Tail)
    |   _ -> Option.None
    
let (|SingleQuoteString|_|) (text: char list) : (string * char list) option =
    let rec loop acc stop tokens =
        match tokens with
        |   '\'' :: rest when stop = tokens.Head -> List.rev ('\'' :: acc), rest
        |   '"' :: rest when stop = tokens.Head -> List.rev ('"' :: acc), rest
        |   [] -> failwith "Unexpected end of string!"
        |   '\r' :: _ | '\n' :: _ -> failwith "Unexpected newline in single quote string!"
        |   c :: rest -> loop (c :: acc) stop rest
            
    match text with
    |   '\'' :: '\'' :: '\'' :: _   -> Option.None (* Triple quote string found *)
    |   '"' :: '"' :: '"' :: _      -> Option.None (* Triple quote string found *)
    |   '\'' :: '\'' :: rest        -> Some("''", rest) (* Empty string *)
    |   '"' :: '"' :: rest          -> Some("\"\"", rest) (* Empty *)
    |   '\'' :: rest                ->
                let text2 , res = loop [ '\'' ] '\'' rest
                let result_text = text2 |> System.String.Concat
                Some(result_text, res)
    |   '"' :: rest                 ->
                let text2 , res = loop [ '"' ] '"' rest
                let result_text = text2 |> System.String.Concat
                Some(result_text, res)
    |   _ -> Option.None
                
let (|MultiQuoteString|_|) (text: char list) : (string * char list) option =
    let rec loop acc stop tokens =
        match tokens with
        |   '\'' :: '\'' :: '\'' :: rest when stop = tokens.Head -> "'''" + string(List.rev acc), rest
        |   '"' :: '"' :: '"' :: rest when stop = tokens.Head -> "\"\"\"" + string(List.rev acc), rest
        |   [] -> failwith "Unexpected end of string!"
        |   c :: rest -> loop (c :: acc) stop rest
        
    match text with
    |   '\'' :: '\'' :: '\'' :: '\'' :: '\'' :: '\'' :: rest    -> Some("''''''", rest) (* Empty string *)
    |   '"' :: '"' :: '"' :: '"' :: '"' :: '"' :: rest          ->Some("\"\"\"\"\"\"", rest) (* Empty string *)
    |   '\'' :: '\'' :: '\'' :: rest                ->
                let text2 , res = loop [ '\''; '\''; '\'' ] '\'' rest
                let result_text = text2 |> System.String.Concat
                Some(result_text, res)
    |   '"' :: '"' :: '"' :: rest                 ->
                let text2 , res = loop [ '"'; '"'; '"' ] '"' rest
                let result_text = text2 |> System.String.Concat
                Some(result_text, res)
    |   _ -> Option.None

let (|SingleOrTripleString|_|) (text: char list, pos: uint) : (Symbol * char list) option =
    let mutable result_text = ""
    let mutable final_rest = text
    
    match text with
    |   Prefix(prefix, rest) ->
            result_text <- prefix
            final_rest <- rest
    |   _ -> ()
            
    match final_rest with
    |   SingleQuoteString(text, rest) ->
            result_text <- result_text + text
            Some(Symbol.String(pos, pos + uint result_text.Length, result_text ), rest)
    |   MultiQuoteString(text, rest) ->
            result_text <- result_text + text
            Some(Symbol.String(pos, pos + uint result_text.Length, result_text ), rest)
    |   _ -> Option.None
    
let (|HexDigit|_|) (text: char list) : (char * char list) option =
    match text with
    |   '0' :: rest -> Some('0', rest)
    |   '1' :: rest -> Some('1', rest)
    |   '2' :: rest -> Some('2', rest)
    |   '3' :: rest -> Some('3', rest)
    |   '4' :: rest -> Some('4', rest)
    |   '5' :: rest -> Some('5', rest)
    |   '6' :: rest -> Some('6', rest)
    |   '7' :: rest -> Some('7', rest)
    |   '8' :: rest -> Some('8', rest)
    |   '9' :: rest -> Some('9', rest)
    |   'a' :: rest | 'A' :: rest -> Some('a', rest)
    |   'b' :: rest | 'B' :: rest -> Some('b', rest)
    |   'c' :: rest | 'C' :: rest -> Some('c', rest)
    |   'd' :: rest | 'D' :: rest -> Some('d', rest)
    |   'e' :: rest | 'E' :: rest -> Some('e', rest)
    |   'f' :: rest | 'F' :: rest -> Some('f', rest)
    |   _ -> Option.None
    
let (|HexNumber|_|) (text: char list, start: uint) : (Symbol * char list) option =
    let rec loop acc tokens =
        match tokens with
        | HexDigit(t, rest)  ->
            match rest with
            |   '_' :: rest2 ->
                    let mutable acc2 = t :: acc
                    acc2 <- '_' :: acc2
                    loop acc2 rest2
            | _ ->
                    loop (t :: acc) rest
        |   '_' :: _ -> failwith "Unexpected underscore!"
        | _ ->
            List.rev acc, tokens
            
    match text, start with
    |   '0' :: 'x' :: '_' ::  rest, _ | '0' :: 'X' :: '_' ::  rest, _ ->
             let res, restFinal = loop [ '_'; 'x'; '0'; ] rest
             let number = System.String.Concat res
             Some(Symbol.Number(start, start + uint(number.Length), number), restFinal)
    |   '0' :: 'x' :: rest, _ | '0' :: 'X' :: rest, _ ->
             let res, restFinal = loop [ 'x'; '0'; ] rest
             let number = System.String.Concat res
             Some(Symbol.Number(start, start + uint(number.Length), number), restFinal)
    |   _ -> Option.None
    
let (|OctetDigit|_|) (text: char list) : (char * char list) option =
    match text with
    |   '0' :: rest -> Some('0', rest)
    |   '1' :: rest -> Some('1', rest)
    |   '2' :: rest -> Some('2', rest)
    |   '3' :: rest -> Some('3', rest)
    |   '4' :: rest -> Some('4', rest)
    |   '5' :: rest -> Some('5', rest)
    |   '6' :: rest -> Some('6', rest)
    |   '7' :: rest -> Some('7', rest)
    |   _ -> Option.None
    
let (|OctetNumber|_|) (text: char list, start: uint) : (Symbol * char list) option =
    let rec loop acc tokens =
        match tokens with
        | OctetDigit(t, rest)  ->
            match rest with
            |   '_' :: rest2 ->
                    let mutable acc2 = t :: acc
                    acc2 <- '_' :: acc2
                    loop acc2 rest2
            | _ ->
                    loop (t :: acc) rest
        |   '8' :: _ |   '9' :: _ -> failwith "Expecting octet number!"
        |   '_' :: _ -> failwith "Unexpected underscore!"
        | _ ->
            List.rev acc, tokens
            
    match text, start with
    |   '0' :: 'o' :: '_' ::  rest, _ | '0' :: 'O' :: '_' ::  rest, _ ->
             let res, restFinal = loop [ '_'; 'o'; '0'; ] rest
             let number = System.String.Concat res
             Some(Symbol.Number(start, start + uint(number.Length), number), restFinal)
    |   '0' :: 'o' :: rest, _ | '0' :: 'O' :: rest, _ ->
             let res, restFinal = loop [ 'o'; '0'; ] rest
             let number = System.String.Concat res
             Some(Symbol.Number(start, start + uint(number.Length), number), restFinal)
    |   _ -> Option.None
    
let (|BinaryDigit|_|) (text: char list) : (char * char list) option =
    match text with
    |   '0' :: rest -> Some('0', rest)
    |   '1' :: rest -> Some('1', rest)
    |   _ -> Option.None

let (|BinaryNumber|_|) (text: char list, start: uint) : (Symbol * char list) option =
    let rec loop acc tokens =
        match tokens with
        | BinaryDigit(t, rest)  ->
            match rest with
            |   '_' :: rest2 ->
                    let mutable acc2 = t :: acc
                    acc2 <- '_' :: acc2
                    loop acc2 rest2
            | _ ->
                    loop (t :: acc) rest
        |  '2' :: _ | '3' :: _ | '4' :: _ | '5' :: _ | '6' :: _ | '7' :: _ | '8' :: _ |   '9' :: _ -> failwith "Expecting binary number!"
        |   '_' :: _ -> failwith "Unexpected underscore!"
        | _ ->
            List.rev acc, tokens
            
    match text, start with
    |   '0' :: 'b' :: '_' ::  rest, _ | '0' :: 'B' :: '_' ::  rest, _ ->
             let res, restFinal = loop [ '_'; 'b'; '0'; ] rest
             let number = System.String.Concat res
             Some(Symbol.Number(start, start + uint(number.Length), number), restFinal)
    |   '0' :: 'b' :: rest, _ | '0' :: 'B' :: rest, _ ->
             let res, restFinal = loop [ 'b'; '0'; ] rest
             let number = System.String.Concat res
             Some(Symbol.Number(start, start + uint(number.Length), number), restFinal)
    |   _ -> Option.None

let (|NonZeroDigit|_|) (text: char list) : (char * char list) option =
    match text with
    |   '1' :: rest -> Some('1', rest)
    |   '2' :: rest -> Some('2', rest)
    |   '3' :: rest -> Some('3', rest)
    |   '4' :: rest -> Some('4', rest)
    |   '5' :: rest -> Some('5', rest)
    |   '6' :: rest -> Some('6', rest)
    |   '7' :: rest -> Some('7', rest)
    |   '8' :: rest -> Some('8', rest)
    |   '9' :: rest -> Some('9', rest)
    |   _ -> Option.None
    
let (|Digit|_|) (text: char list) : (char * char list) option =
    match text with
    |   '0' :: rest -> Some('0', rest)
    |   NonZeroDigit(t, rest) -> Some(t, rest)
    |   _ -> Option.None
    
let (|Imaginary|_|) (text: char list) : (char list) option =
    match text with
    |   'j' :: rest -> Some(rest)
    |   'J' :: rest -> Some(rest)
    |   _ -> Option.None
    
let (|Exponent|_|) (text: char list) : (char list) option =
    match text with
    |   'e' :: rest -> Some(rest)
    |   'E' :: rest -> Some(rest)
    |   _ -> Option.None
    
let (|Signed|_|) (text: char list) : (char * char list) option =
    match text with
    |   '+' :: rest -> Some('+', rest)
    |   '-' :: rest -> Some('-', rest)
    |   _ -> Option.None
    
let (|ExponentPart|_|) (text: char list) : (string * char list) option =
    let rec loop acc tokens =
        match tokens with
        | Digit(t, rest)  ->
            match rest with
            |   '_' :: rest2 ->
                    let mutable acc2 = t :: acc
                    acc2 <- '_' :: acc2
                    loop acc2 rest2
            | _ ->
                    loop (t :: acc) rest
        |   '_' :: _ -> failwith "Unexpected underscore!"
        | _ ->
            acc, tokens
            
    match text with
    |   Exponent(r)  ->
            match r with
            |   Signed(c1, r2) ->
                    match r2 with
                    |   Digit( _, _) ->
                            let mutable result, r3 = loop [ c1; 'e'; ] r2
                            match r3 with
                            |   Imaginary(r4) ->
                                    result <- 'j' :: result
                                    let result2 = List.rev result
                                    Some( result2 |> System.String.Concat , r4 )
                            |   _ ->
                                let result2 = List.rev result
                                Some( result2 |> System.String.Concat , r3 )
                    |   _ -> failwith "Expecting digit after exponent!"   
            |    Digit( _, _) ->
                            let mutable result, r3 = loop [ 'e'; ] r
                            match r3 with
                            |   Imaginary(r4) ->
                                    result <- 'j' :: result
                                    let result2 = List.rev result
                                    Some( result2 |> System.String.Concat , r4 )
                            |   _ ->
                                let result2 = List.rev result
                                Some( result2 |> System.String.Concat , r3 )
            |   _ -> failwith "Expecting digit after exponent!"   
    |   _ -> Option.None
    
let (|NumberStartingWithZero|_|) (text: char list, pos: uint) : (Symbol * char list) option =
    let rec loop acc tokens =
        match tokens with
        | Digit(t, rest)  ->
            match rest with
            |   '_' :: rest2 ->
                    let mutable acc2 = t :: acc
                    acc2 <- '_' :: acc2
                    loop acc2 rest2
            | _ ->
                    loop (t :: acc) rest
        |   '_' :: _ -> failwith "Unexpected underscore!"
        | _ ->
            acc, tokens
            
    let rec loop_zero acc tokens =
        match tokens with
        | '0' :: rest  ->
            match rest with
            |   '_' :: rest2 ->
                    let mutable acc2 = '0' :: acc
                    acc2 <- '_' :: acc2
                    loop_zero acc2 rest2
            | _ ->
                    loop_zero ('0' :: acc) rest
        |   '_' :: _ -> acc, tokens
        | _ ->
            acc, tokens
            
    match text with
    |   '0' :: _ ->
            let result, r2 = loop_zero [] text (* Collect all leading zeros *)
            let mutable text_result = List.rev result |> System.String.Concat
            let mutable final_rest = r2
            
            match final_rest with
            |   Digit _ -> failwith "Unexpected nonzero digit after leading zeros!"
            |   ExponentPart(s, r3) ->
                    text_result <- text_result + s
                    final_rest <- r3
            |   Imaginary(r3) ->
                    text_result <- text_result + "j"
                    final_rest <- r3
            |   '.' :: r ->
                    let mutable start_list = [ '.' ]
                    
                    match r with
                    |   '_' :: rest2 ->
                            final_rest <- rest2
                            start_list <- '_' :: start_list
                    |   _ ->
                            final_rest <- r
                    
                    let result, r2 = loop start_list final_rest
                          
                    text_result <- text_result + (List.rev result |> System.String.Concat)
                    final_rest <- r2
                    match r2 with
                    |   ExponentPart(s, r3) ->
                            text_result <- text_result + s
                            final_rest <- r3
                    |   Imaginary(r3) ->
                            text_result <- text_result + "j"
                            final_rest <- r3
                    |   _ ->
                            final_rest <- r2       
            |   _ -> ()
        
            Some(Symbol.Number(pos, pos + uint(text_result.Length), text_result), final_rest)
    |   _ ->    Option.None
    
let (|NumberStartingWithNonZero|_|) (text: char list, pos: uint) : (Symbol * char list) option =
    let rec loop acc tokens =
        match tokens with
        | Digit(t, rest)  ->
            match rest with
            |   '_' :: rest2 ->
                    let mutable acc2 = t :: acc
                    acc2 <- '_' :: acc2
                    loop acc2 rest2
            | _ ->
                    loop (t :: acc) rest
        |   '_' :: _ -> failwith "Unexpected underscore!"
        | _ ->
            acc, tokens
    
    match text with
    |   NonZeroDigit _ ->
            let result, r2 = loop [] text
            let mutable text_result = List.rev result |> System.String.Concat
            let mutable final_rest = r2
            
            match final_rest with
            |   ExponentPart(s, r3) ->
                    text_result <- text_result + s
                    final_rest <- r3
            |   Imaginary(r3) ->
                    text_result <- text_result + "j"
                    final_rest <- r3
            |   '.' :: r ->
                    let mutable start_list = [ '.' ]
                    
                    match r with
                    |   '_' :: rest2 ->
                            final_rest <- rest2
                            start_list <- '_' :: start_list
                    |   _ ->
                            final_rest <- r
                    
                    let result, r2 = loop start_list final_rest
                          
                    text_result <- text_result + (List.rev result |> System.String.Concat)
                    final_rest <- r2
                    match r2 with
                    |   ExponentPart(s, r3) ->
                            text_result <- text_result + s
                            final_rest <- r3
                    |   Imaginary(r3) ->
                            text_result <- text_result + "j"
                            final_rest <- r3
                    |   _ ->
                            final_rest <- r2       
            |   _ -> ()
        
            Some(Symbol.Number(pos, pos + uint(text_result.Length), text_result), final_rest)
    |   _ ->    Option.None
    
let (|NumberStartingWithPeriod|_|) (text: char list, pos: uint) : (Symbol * char list) option =
    let rec loop acc tokens =
        match tokens with
        | Digit(t, rest)  ->
            match rest with
            |   '_' :: rest2 ->
                    let mutable acc2 = t :: acc
                    acc2 <- '_' :: acc2
                    loop acc2 rest2
            | _ ->
                    loop (t :: acc) rest
        |   '_' :: _ -> failwith "Unexpected underscore!"
        | _ ->
            acc, tokens
            
    match text with
    |   '.' :: Digit(c, r) ->
            let mutable restx = r
            let mutable start_list = [ c; '.' ]
            match r with
            |   '_' :: rest2 ->
                    restx <- rest2
                    start_list <- '_' :: start_list
            |   _ ->
                    restx <- r
            
            let result, r2 = loop start_list restx
                  
            let mutable text_result = List.rev result |> System.String.Concat
            let mutable final_rest = r2
            match r2 with
            |   ExponentPart(s, r3) ->
                    text_result <- text_result + s
                    final_rest <- r3
            |   Imaginary(r3) ->
                    text_result <- text_result + "j"
                    final_rest <- r3
            |   _ ->
                    final_rest <- r2
            
            Some(Symbol.Number(pos, pos + uint(text_result.Length), text_result), final_rest)
    |   _ -> Option.None

let (|Number|_|) (text: char list, start: uint) : (Symbol * char list) option =
    match (text, start) with
    |   HexNumber(s, r) -> Some(s, r)
    |   OctetNumber(s, r) -> Some(s, r)
    |   BinaryNumber(s, r) -> Some(s, r)
    |   NumberStartingWithZero(s, r) -> Some(s, r)
    |   NumberStartingWithNonZero(s, r) -> Some(s, r)
    |   NumberStartingWithPeriod(s, r) -> Some(s, r)
    |   _ -> Option.None
    
    
let Tokenize(text: char list) : SymbolStream =
    let size = uint(text.Length)
    let mutable elements : SymbolStream = []
    let mutable source = text
    let mutable parenthesis_stack = []
    let mutable at_bol = true
    let mutable level = 0
    let mutable blank_line = false
    let mutable indent_stack : uint list = [ 0u; ]
    let mutable pending = 0
    let tab_size = 4u
    
    (* Next line handling *)
    while source.Length > 0 do
        
        if at_bol then
            at_bol <- false
            let mutable col = 0u
            while source.Length > 0 &&
                  match source with
                  | ' ' :: rest -> source <- rest; col <- col + 1u; true
                  | '\t' :: rest  -> source <- rest; col <- (col / tab_size + 1u) * tab_size; true
                  | '\v' :: rest  -> source <- rest; col <- col + 1u; true
                  | _ -> false
                do ()
                
            (* Check for blank line *)
            if col = 0u &&
                   match source with | '\n' :: _  | '\r' :: _  | '#' :: _ -> true | _ -> false
                   then ()  (* FIX LATER! *)
                
            (* Analyze indentation level *)
            if blank_line = false && level = 0 then
                if col > indent_stack.Head then
                    indent_stack <- col :: indent_stack
                    pending <- pending + 1
                else if col < indent_stack.Head then
                    while indent_stack.IsEmpty = false && col < indent_stack.Head do
                        pending <- pending - 1
                        indent_stack <- indent_stack.Tail
                    if indent_stack.IsEmpty = false then
                        failwith "Mismatched indentation level!"
                
            (* Handling indentation or dedentation *)
            if pending <> 0 then
                if pending > 0 then
                        elements <-Symbol.Indent(size - uint(source.Length)) :: elements
                        pending <- pending - 1
                else
                    while indent_stack.Length <0 do
                        elements <- Symbol.Dedent(size - uint(source.Length)) :: elements
                        pending <- pending + 1
    
        (* Again loop *)
        while source.Length > 0 && at_bol = false do
            
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
                    match s with
                    | Symbol.LeftParenthesis _ ->
                            parenthesis_stack <- '(' :: parenthesis_stack
                            level <- level + 1
                    | Symbol.RightParenthesis _ ->
                        match parenthesis_stack with
                        | '(' :: rest->
                                parenthesis_stack <- rest
                                level <- level - 1
                        | _ -> failwith "Unexpected right parenthesis!"
                    | Symbol.LeftSquareBracket _ ->
                            parenthesis_stack <- '[' :: parenthesis_stack
                            level <- level + 1
                    | Symbol.RightSquareBracket _ ->
                        match parenthesis_stack with
                        | '[' :: rest->
                                parenthesis_stack <- rest
                                level <- level - 1
                        | _ -> failwith "Unexpected right bracket parenthesis!"
                    | Symbol.LeftCurlyBracket _ ->
                            parenthesis_stack <- '{' :: parenthesis_stack
                            level <- level + 1
                    | Symbol.RightCurlyBracket _ ->
                        match parenthesis_stack with
                        | '{' :: rest->
                                parenthesis_stack <- rest
                                level <- level - 1
                        | _ -> failwith "Unexpected right parenthesis!"
                    | _ -> ()
            |   ReservedKeywordOrLiteral(s, r) ->
                    elements <- s :: elements
                    source <- r
            |   SingleOrTripleString(s, r) ->
                    elements <- s :: elements
                    source <- r
            |   '\\' :: '\r' :: '\n' :: rest, _  |  '\\' :: '\n' :: rest, _  |  '\\':: '\r' :: rest, _ ->
                    source <- rest
            |   '\\' :: _ , _ -> failwith "Unexpected backslash not followed by newline!"
            |   '\r' :: '\n' :: rest, _  |  '\n' :: rest, _ | '\r' :: rest, _ -> source <- rest
            |   '#' ::  ' ' :: 't' :: 'y' :: 'p' :: 'e' ::':' :: rest, _    -> source <- rest
            |   '#' ::  rest, _  -> source <- rest
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
