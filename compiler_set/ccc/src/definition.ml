type type_class = Void | Char | Int | Long | Float | Signed | Unsigned
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

type 'a parameter =
    Parameter of type_class * 'a
  | PointerParameter of type_class * 'a (* pointer comes only in parameters *)
    deriving (Show)

let parameter_id = function
  | Parameter(_, id) -> id
  | PointerParameter(_, id) -> id

let parameter_type = function
  | Parameter(typ, _) -> convert_syntactic_type typ
  | PointerParameter(typ, _) -> Type.Array(convert_syntactic_type typ)

type ('a, 'b) variable =
    Variable of 'a * type_class * 'b
    deriving (Show)

type 'a global_variable = ('a, const_value) variable
    deriving (Show)

type 'a function_signature = { name: Id.l; return_type: type_class; parameters: 'a parameter list }
    deriving (Show)

type 'a array_signature = { id: 'a; content_type: type_class; size: int }
    deriving (Show)
