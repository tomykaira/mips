open Definition

type statement =
  | Label  of Id.l
  | Exp    of Id.v Syntax.exp
  | Block  of Id.v variable list * statement list
  | IfEq   of Id.v Syntax.exp * Id.v Syntax.exp * statement * statement option
  | IfLt   of Id.v Syntax.exp * Id.v Syntax.exp * statement * statement option
  | IfTrue of Id.v Syntax.exp * statement * statement option
  | Switch of Id.v Syntax.exp * switch_case list
  | Goto   of Id.l
  | Continue
  | Break
  | Return of Id.v Syntax.exp option
and switch_case =
  | SwitchCase  of const_value * statement list
  | DefaultCase of statement list
    deriving (Show)

type t =
  | Function of Id.v function_signature * statement
  | FunctionDeclaration of Id.v function_signature
  | GlobalVariable of Id.v variable
  | Array of Id.v array_signature
    deriving (Show)

val convert : Id.v MacroExpand.t list -> t list
