module SimplyPythonNet.tokenizer

type Token =
    | Empty
    

let OneCharToken c =
    match c with
    |   '!' -> Some()
    |   _ -> None
    
let TwoCharToken (c1, c2) =
     match c1 c2 with
     | '!', '=' -> Some()
     |   _ -> None
     
let ThreeCharToken (c1, c2, c3)=
     match c1 c2 c3 with
     | '<', '<', '=' -> Some()
     |   _ -> None
     
let ReservedKeyword word =
    match word with
    |   "False" -> Some()
    | _ -> None