type id = int
      deriving (Show)

type 'a entity = E of (id * 'a)
      deriving (Show)

val identify : 'a list -> 'a entity list

val unidentify : 'a entity list -> 'a list
