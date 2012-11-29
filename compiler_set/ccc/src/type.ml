
type t =
  | Void
  | Int
  | Char
  | Float
  | Fun of t * t list
      deriving (Show)
