open Definition

type statement =
  | Label  of Id.l
  | Exp    of Id.v Syntax.exp
  | Block  of Id.v variable list * statement list
  | IfEq   of Id.v Syntax.exp * Id.v Syntax.exp * statement * statement option
  | IfLt   of Id.v Syntax.exp * Id.v Syntax.exp * statement * statement option
  | IfTrue of Id.v Syntax.exp * statement * statement option
  | Goto   of Id.l
  | Return of Id.v Syntax.exp option
    deriving (Show)

type t =
  | Function of Id.v function_signature * statement
  | FunctionDeclaration of Id.v function_signature
  | GlobalVariable of Id.v variable
  | Array of Id.v array_signature
    deriving (Show)

val convert : Id.v MacroExpand.t list -> t list
