module SimplyPythonNet.tokenizer

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
    
    

    

let OneCharToken c =
    match c with
    |   '!' -> Some(Token.Empty)
    |   _ -> Option.None
    
let TwoCharToken c1 c2 =
     match c1 c2 with
     | '!', '=' -> Some(Token.Empty)
     |   _ -> Option.None
     
let ThreeCharToken c1 c2 c3=
     match c1 c2 c3 with
     | '<', '<', '=' -> Some(Token.Empty)
     |   _ -> Option.None
     
let ReservedKeyword word =
    match word with
    |   "False" -> Some(Token.False)
    |   "None" -> Some(Token.None)
    |   "True" -> Some(Token.True)
    |   "and" -> Some(Token.And)
    |   "as" -> Some(Token.As)
    |   "assert" -> Some(Token.Assert)
    |   "async" -> Some(Token.Async)
    |   "await" -> Some(Token.Await)
    |   "break" -> Some(Token.Break)
    |   "class" -> Some(Token.Class)
    |   "continue" -> Some(Token.Continue)
    |   "def" -> Some(Token.Def)
    |   "del" -> Some(Token.Del)
    |   "elif" -> Some(Token.Elif)
    |   "else" -> Some(Token.Else)
    |   "except" -> Some(Token.Except)
    |   "finally" -> Some(Token.Finally)
    |   "for" -> Some(Token.For)
    |   "from" -> Some(Token.From)
    |   "global" -> Some(Token.Global)
    |   "if" -> Some(Token.If)
    |   "in" -> Some(Token.In)
    |   "is" -> Some(Token.Is)
    |   "lambda" -> Some(Token.Lambda)
    |   "nonlocal" -> Some(Token.Nonlocal)
    |   "not" -> Some(Token.Not)
    |   "or" -> Some(Token.Or)
    |   "pass" -> Some(Token.Pass)
    |   "raise" -> Some(Token.Raise)
    |   "return" -> Some(Token.Return)
    |   "try" -> Some(Token.Try)
    |   "while" -> Some(Token.While)
    |   "with" -> Some(Token.With)
    |   "yield" -> Some(Token.Yield)
    |   _ -> Option.None