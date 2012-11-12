(*pp deriving *)

val danger : Id.t list ref

type closure = { entry : Id.l; actual_fv : Id.t list }
    deriving (Show)
type t =
  | Unit
  | Int of int
  | Float of float
  | Neg of Id.t
  | Add of Id.t * Id.t
  | Sub of Id.t * Id.t
  | Mul of Id.t * Id.t
  | Sll of Id.t * int
  | Sra of Id.t * int
  | FNeg of Id.t
  | FAdd of Id.t * Id.t
  | FSub of Id.t * Id.t
  | FMul of Id.t * Id.t
  | FDiv of Id.t * Id.t
  | IfEq of Id.t * Id.t * t * t
  | IfLE of Id.t * Id.t * t * t
  | IfLT of Id.t * Id.t * t * t
  | IfNil of Id.t * t * t
  | Let of (Id.t * Type.t) * t * t
  | Var of Id.t
  | MakeCls of (Id.t * Type.t) * closure * t
  | AppCls of Id.t * Id.t list
  | AppDir of Id.l * Id.t list
  | Tuple of Id.t list
  | LetTuple of (Id.t * Type.t) list * Id.t * t
  | Get of Id.t * Id.t
  | Put of Id.t * Id.t * Id.t
  | ExtArray of Id.l
  | Nil
  | Cons of Id.t * Id.t
  | LetList of (Syntax.list_matcher * Type.t) * Id.t * t
      deriving (Show)
type fundef = { name : Id.l * Type.t;
                args : (Id.t * Type.t) list;
                formal_fv : (Id.t * Type.t) list;
                body : t }
    deriving (Show)
type prog = Prog of fundef list * t
    deriving (Show)

val fv : t -> S.t
val f : KNormal.t -> prog
