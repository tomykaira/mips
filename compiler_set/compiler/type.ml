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

let gentyp () = Var(ref None) (* 新しい型変数を作る *)

let show = Show.show<t>
