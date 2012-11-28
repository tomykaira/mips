module BoundedStore (BoundedStore, create, add, toList) where

import Data.Array.IArray

data BoundedStore a = BoundedStore { size    :: Int
                                   , pointer :: Int
                                   , store   :: Array Int a }

create :: Int -> BoundedStore a
create argSize =
    BoundedStore { size = argSize, pointer = 0, store = array (0, argSize-1) [] }

add :: a -> BoundedStore a -> BoundedStore a
add thing s =
    s { pointer = newPointer, store = store s // [(pointer s, thing)] }
    where
      newPointer = pointer s + 1 `mod` size s

toList :: BoundedStore a -> [a]
toList s =
    map (\i -> store s ! (i `mod` (size s))) $ enumFromTo (pointer s) (pointer s + size s)
