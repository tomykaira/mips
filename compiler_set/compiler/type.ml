type t = (* MinCamlの型を表現するデータ型 *)
  | Unit
  | Bool
  | Int
  | Float
  | Fun of t list * t * bool (* arguments are uncurried *)
  | Tuple of t list * bool
  | Array of t * bool
  | Var of t option ref

let rec show x =
  match x with
  | Unit -> "Unit"
  | Bool -> "Bool"
  | Int -> "Int"
  | Float -> "Float"
  | Fun (l,t, b) -> "Fun((" ^ String.concat "," (List.map show l) ^ ")->" ^ show t ^ ")" ^ string_of_bool b
  | Tuple (l, b) -> "(" ^ String.concat " * " (List.map show l) ^")"^ string_of_bool b
  | Array (t, b) -> "(Array " ^ show t ^ ")" ^ string_of_bool b
  | Var a ->
      (match !a with
       | Some t -> "Var " ^ show t
       | None -> "Var")

let gentyp () = Var(ref None) (* 新しい型変数を作る *)

