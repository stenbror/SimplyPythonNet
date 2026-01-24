module SimplyPythonNet.ActivePatternParser

type Symbol =
    | Name of uint * uint * string
    | Number of uint * uint * string
    | Plus of uint * uint
    
type AST =
    | Name of uint * uint * string
    | Number of uint * uint * string
    | Plus of AST * Symbol * AST
    
type SymbolStream = Symbol list
type NodeTree = AST * SymbolStream

(* Expression patterns  *)

let (|Atom|) (stream : SymbolStream) : NodeTree =
    match stream with
    |   Symbol.Name(s, e, t) :: rest -> AST.Name(s, e, t), rest
    |   Symbol.Number(s, e, t) :: rest -> AST.Number(s, e, t), rest
    | _ -> failwith "Expecting an expression value!"