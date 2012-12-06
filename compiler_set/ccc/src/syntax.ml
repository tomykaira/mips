open Definition

(*pp deriving *)
type storage_class = Auto | Register | Static | Extern | Typedef
    deriving (Show)

type 'a assignee =
  | VarSet of 'a
  | ArraySet of 'a * 'a exp
and 'a exp =
  | Var            of 'a
  | Const          of const_value
  | ArrayRef       of 'a * 'a exp
  | Assign         of 'a assignee * 'a exp  (* TODO: refine first arg *)
  | And            of 'a exp * 'a exp
  | Or             of 'a exp * 'a exp
  | Equal          of 'a exp * 'a exp
  | LessThan       of 'a exp * 'a exp
  | Add            of 'a exp * 'a exp
  | Sub            of 'a exp * 'a exp
  | Mul            of 'a exp * 'a exp
  | Div            of 'a exp * 'a exp
  | Mod            of 'a exp * 'a exp
  | Not            of 'a exp
  | Negate         of 'a exp
  | CallFunction   of Id.l * 'a exp list
      deriving (Show)

(* Convert assignee to exp for parser *)
let ref_of = function
  | VarSet(v) -> Var(v)
  | ArraySet(a, e) -> ArrayRef(a, e)

type 'a statement =
  | Label  of Id.l
  | Exp    of 'a exp
  | Block  of 'a variable list * 'a statement list
  | If     of 'a exp * 'a statement * 'a statement option
  | Switch of 'a exp * 'a switch_case list
  | While  of 'a exp * 'a statement
  | Goto   of Id.l
  | Continue
  | Break
  | Return of 'a exp option
and 'a switch_case =
  | SwitchCase  of const_value * 'a statement
  | DefaultCase of 'a statement
    deriving (Show)

type macro = ConstMacro of Id.t * const_value | ExpMacro of Id.t * Id.t list * Id.t exp
    deriving (Show)

let signature id return_type params =
  { name = Id.L id; return_type = return_type; parameters = params }

type 'a t =
  | Function of 'a function_signature * 'a statement
  | FunctionDeclaration of 'a function_signature
  | GlobalVariable of 'a variable
  | Array of 'a array_signature
  | DefineMacro of macro
    deriving (Show)
