type t =
  | Void
  | Int
  | Char
  | Float
      deriving (Show)

type fun_type = Fun of t * t list
