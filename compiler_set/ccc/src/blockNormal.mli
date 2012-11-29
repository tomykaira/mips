type statement =
  | Label  of Id.l
  | Exp    of Syntax.exp
  | If     of exp * block * block option
  | Switch of exp * switch_case list
  | While  of exp * block
  | Goto   of Id.l
  | Continue
  | Break
  | Return of exp option
and switch_case =
  | SwitchCase  of const_value * block
  | DefaultCase of block
and block =
  | Block of Syntax.variable list * statement list

type t =
  | Function of Id.t * type_class * Syntax.parameter list * block
  | GlobalVariable of Syntax.variable

val normalize : Syntax.t list -> t list
