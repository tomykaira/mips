open Definition

(*pp deriving *)
type storage_class = Auto | Register | Static | Extern | Typedef
    deriving (Show)

type assignee =
  | VarSet of Id.v
  | ArraySet of Id.v * exp
and exp =
  | Var            of Id.v
  | Const          of const_value
  | ArrayRef       of Id.v * exp
  | Assign         of assignee * exp  (* TODO: refine first arg *)
  | And            of exp * exp
  | Or             of exp * exp
  | Equal          of exp * exp
  | LessThan       of exp * exp
  | GreaterThan    of exp * exp
  | Add            of exp * exp
  | Sub            of exp * exp
  | Mul            of exp * exp
  | Div            of exp * exp
  | Mod            of exp * exp
  | Not            of exp
  (* | Address        of exp *)
  (* | Reference      of exp *)
  | Negate         of exp
  (* | ArrayReference of exp * exp *)
  | CallFunction   of Id.l * exp list
    deriving (Show)

(* Convert assignee to exp for parser *)
let ref_of = function
  | VarSet(v) -> Var(v)
  | ArraySet(a, e) -> ArrayRef(a, e)

type statement =
  | Label  of Id.l * statement
  | Exp    of exp
  | Block  of variable list * statement list
  | If     of exp * statement * statement option
  | Switch of exp * switch_case list
  | While  of exp * statement
  | Goto   of Id.l
  | Continue
  | Break
  | Return of exp option
and switch_case =
  | SwitchCase  of const_value * statement
  | DefaultCase of statement
    deriving (Show)

type macro = ConstMacro of Id.t * const_value | ExpMacro of Id.t * Id.t list * exp
    deriving (Show)

let signature id return_type params =
  { name = Id.L id; return_type = return_type; parameters = params }

type t =
  | Function of function_signature * statement
  | FunctionDeclaration of function_signature
  | GlobalVariable of variable
  | Array of array_signature
  | DefineMacro of macro
    deriving (Show)
