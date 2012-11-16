(*pp deriving *)
(* MinCamlの構文を表現するデータ型 *)
type t =
  | Unit
  | Bool of bool
  | Int of int
  | Float of float
  | Not of t
  | Neg of t
  | Add of t * t
  | Sub of t * t
  | Mul of t * t
  | Sll of t * int
  | Sra of t * int
  | FNeg of t
  | FAdd of t * t
  | FSub of t * t
  | FMul of t * t
  | FDiv of t * t
  | Eq of t * t
  | LE of t * t
  | LT of t * t
  | If of t * t * t
  | Let of (Id.t * Type.t) * t * t
  | Var of Id.t
  | LetRec of fundef * t
  | App of t * t list
  | Tuple of t list
  | LetTuple of (Id.t * Type.t) list * t * t
  | Array of t * t
  | Get of t * t
  | Put of t * t * t
  | Match of t * (pattern * t) list
  | Nil
  | Cons of t * t
  | IsNil of t
  | LetList of (list_matcher * Type.t option ref) * t * t
and fundef = { name : Id.t * Type.t; args : (Id.t * Type.t) list; body : t }
and pattern = IntPattern of int | VarPattern of Id.t
and list_matcher = ListWithNil of Id.t list | ListWithoutNil of Id.t list
    deriving (Show)

let matcher_variables = function ListWithNil(vars) -> vars | ListWithoutNil(vars) -> vars

let add_type_variables matcher typ =
  match matcher with
    | ListWithNil(variables) -> List.map (fun v -> (v, Type.Var(typ))) variables
    | ListWithoutNil(variables) ->
      let reversed   = List.rev variables in
      let list_var   = List.hd reversed in
      let other_vars = List.tl reversed in
      (list_var, Type.List(typ)) :: (List.map (fun v -> (v, Type.Var(typ))) other_vars)
