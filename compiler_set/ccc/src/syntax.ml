(*pp deriving *)
type storage_class = Auto | Register | Static | Extern | Typedef
    deriving (Show)

type type_class = Void | Char | Int | Long | Float | Signed | Unsigned (* | UserDefined of Id.t *)
    deriving (Show)

(* type type_qualifier = Const | Volatile *)
let convert_syntactic_type = function
  | Void     -> Type.Void
  | Char     -> Type.Char
  | Int      -> Type.Int
  | Long     -> Type.Int
  | Float    -> Type.Float
  | Signed   -> Type.Int
  | Unsigned -> Type.Int


type const_value =
  | IntVal of int | CharVal of char | FloatVal of float
    deriving (Show)

let const_type = function
  | IntVal _ -> Type.Int
  | CharVal _ -> Type.Char
  | FloatVal _ -> Type.Float


type variable =
    Define of Id.t * type_class * const_value
    deriving (Show)

type exp =
  | Var            of Id.t
  | Const          of const_value
  | Assign         of exp * exp  (* TODO: refine first arg *)
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
  | PostIncrement  of exp
  | PostDecrement  of exp
    deriving (Show)

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

type parameter =
    Parameter of type_class * Id.t
    deriving (Show)

type t =
  | Function of Id.l * type_class * parameter list * statement
  | GlobalVariable of variable
    deriving (Show)
