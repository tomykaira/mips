type t =
  | Void
  | Int
  | Char
  | Float
  | Array of t
      deriving (Show)

type fun_type = Fun of t * t list
      deriving (Show)
