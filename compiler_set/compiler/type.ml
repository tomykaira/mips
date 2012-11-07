(*pp deriving *)
type t = (* MinCamlの型を表現するデータ型 *)
  | Unit
  | Bool
  | Int
  | Float
  | Fun of t list * t (* arguments are uncurried *)
  | Tuple of t list
  | Array of t
  | Var of t option ref
  | List of t option ref
      deriving (Show)

let rec show x =
  match x with
  | Unit -> "Unit"
  | Bool -> "Bool"
  | Int -> "Int"
  | Float -> "Float"
  | Fun (l,t) -> "(" ^ String.concat "," (List.map show l) ^ ")->" ^ show t
  | Tuple l -> String.concat " * " (List.map show l)
  | Array t -> "Array " ^ show t
  | Var a ->
      (match !a with
       | Some t -> "Var " ^ show t
       | None -> "Var")
  | List a ->
      (match !a with
       | Some t -> "List of " ^ show t
       | None -> "List")

let gentyp () = Var(ref None) (* 新しい型変数を作る *)

