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
    (* Operators *)
    |   PlusAssign of uint * uint
    |   MinusAssign of uint * uint
    |   MulAssign of uint * uint
    |   PowerAssign of uint * uint
    |   SlashAssign of uint * uint
    |   DoubleSlashAssign of uint * uint
    |   ModuloAssign of uint * uint
    |   BitwiseAndAssign of uint * uint
    |   BitwiseOrAssign of uint * uint
    |   BitwiseXorAssign of uint * uint
    |   BitwiseShiftLeftAssign of uint * uint
    |   BitwiseShiftRightAssign of uint * uint
    |   MatricesAssign of uint * uint
    |   ColonAssign of uint * uint
    |   BitwiseAnd of uint * uint
    |   BitwiseOr of uint * uint
    |   BitwiseXor of uint * uint
    |   BitwiseInvert of uint * uint
    |   BitwiseShiftLeft of uint * uint
    |   BitwiseShiftRight of uint * uint
    |   LessEqual of uint * uint
    |   GreaterEqual of uint * uint
    |   Less of uint * uint
    |   Greater of uint * uint
    |   Equal of uint * uint
    |   NotEqual of uint * uint
    |   LeftParen of uint * uint
    |   RightParen of uint * uint
    |   LeftBracket of uint * uint
    |   RightBracket of uint * uint
    |   LeftCurly of uint * uint
    |   RightCurly of uint * uint
    |   Comma of uint * uint
    |   Colon of uint * uint
    |   BitwiseNot of uint * uint
    |   SemiColon of uint * uint
    |   Assign of uint * uint
    |   Arrow of uint * uint
    |   Plus of uint * uint
    |   Minus of uint * uint
    |   Power of uint * uint
    |   Mul of uint * uint
    |   DoubleSlash of uint * uint
    |   Slash of uint * uint
    |   Modulo of uint * uint
    |   Period of uint * uint
    |   Matrices of uint * uint
    |   Ellipsis of uint * uint
    
let OneCharToken c =
    match c with
    |   '&' -> Some(Token.BitwiseAnd)
    |   '|' -> Some(Token.BitwiseOr)
    |   '^' -> Some(Token.BitwiseXor)
    |   '~' -> Some(Token.BitwiseInvert)
    |   '<' -> Some(Token.Less)
    |   '>' -> Some(Token.Greater)
    |   '(' -> Some(Token.LeftParen)
    |   ')' -> Some(Token.RightParen)
    |   '[' -> Some(Token.LeftBracket)
    |   ']' -> Some(Token.RightParen)
    |   '{' -> Some(Token.LeftCurly)
    |   '}' -> Some(Token.RightCurly)
    |   ',' -> Some(Token.Comma)
    |   ':' -> Some(Token.Colon)
    |   '!' -> Some(Token.BitwiseNot)
    |   ';' -> Some(Token.SemiColon)
    |   '=' -> Some(Token.Assign)
    |   '+' -> Some(Token.Plus)
    |   '-' -> Some(Token.Minus)
    |   '*' -> Some(Token.Mul)
    |   '/' -> Some(Token.Slash)
    |   '%' -> Some(Token.Modulo)
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